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
  users <- readUsers fp

  lift $ putStrLn "Loading channels..."
  chans <- readChannels fp

  lift $ putStrLn "Loading channel messages.."
  chans' <- mapM (loadChannelMessages fp) chans

  -- less readable?
  -- fmap (mkSlackArchive users) $ mapM (MaybeT . loadChannelMessages fp) chans
  return $ mkSlackArchive users chans'

readUsers :: FilePath -> MaybeT IO ([User])
readUsers fp = MaybeT . parseFile $ fp ++ "/users.json"

readChannels :: FilePath -> MaybeT IO ([Channel])
readChannels fp = MaybeT . parseFile $ fp ++ "/channels.json"

loadChannelMessages :: FilePath -> Channel -> MaybeT IO (Channel)
loadChannelMessages fp (Channel cId cName _) =
  do
    channelFiles <- lift $ getDirectoryContents basePath
    chatMsgs <- readMessages $ (prefixBasePath . filterJsonFiles) channelFiles basePath

    -- why does this work
    return $ Channel cId cName chatMsgs
  where
    basePath = fp ++ "/" ++ cName ++ "/"

    filterJsonFiles :: [FilePath] -> [FilePath]
    filterJsonFiles = filter (".json" `isSuffixOf`)

    prefixBasePath :: [FilePath] -> FilePath -> [FilePath]
    prefixBasePath fps prefix = map ((++) prefix) fps

readMessages :: [FilePath] -> MaybeT IO ([ChatMessage])
readMessages fps =
  do
    fmap concat $ mapM (MaybeT . readMessage) fps
  where
    readMessage :: FilePath -> IO (Maybe [ChatMessage])
    readMessage = parseFile

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fp = decode <$> BS.readFile fp