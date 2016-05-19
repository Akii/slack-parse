module Slack.Internal.Parsing (load) where

import            Prelude
import            Data.Aeson
import qualified  Data.ByteString.Lazy as BS
import            Data.List (isSuffixOf)
import            Control.Monad.Trans.Maybe
import            Control.Monad.Trans.Class (lift)
import            System.Directory (getDirectoryContents)

import Slack.Internal.Types

load :: FilePath -> IO (Maybe SlackArchive)
load fp = runMaybeT $ do
  lift $ putStrLn "Loading users..."
  users <- MaybeT (readUsers fp)

  lift $ putStrLn "Loading channels..."
  chans <- MaybeT (readChannels fp)

  lift $ putStrLn "Loading channel messages.."
  chans' <- mapM (\c -> MaybeT (loadChannelMessages fp c)) chans

  return $ mkSlackArchive users chans'

readUsers :: FilePath -> IO (Maybe [User])
readUsers fp = parseFile $ fp ++ "/users.json"

readChannels :: FilePath -> IO (Maybe [Channel])
readChannels fp = parseFile $ fp ++ "/channels.json"

loadChannelMessages :: FilePath -> Channel -> IO (Maybe Channel)
loadChannelMessages fp (Channel cId name _) = do
  let bpath = fp ++ "/" ++ name ++ "/"

  dirs <- getDirectoryContents bpath

  let jsonFiles = map ((++) bpath) $ filter (".json" `isSuffixOf`) dirs

  parsed <- readMessages jsonFiles

  return $ fmap (Channel cId name) parsed

readMessages :: [FilePath] -> IO (Maybe [ChatMessage])
readMessages fps =
  do
    runMaybeT $ fmap concat $ mapM (MaybeT . readMessage) fps
  where
    readMessage :: FilePath -> IO (Maybe [ChatMessage])
    readMessage = parseFile

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fp = decode <$> BS.readFile fp