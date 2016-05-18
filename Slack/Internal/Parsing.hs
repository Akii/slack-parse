module Slack.Internal.Parsing (load) where

import            Prelude
import            Data.Aeson
import qualified  Data.ByteString.Lazy as BS
import            Control.Monad.Trans.Maybe
import            Control.Monad.Trans.Class (lift)
import            System.Directory (getDirectoryContents)

import Slack.Internal.Types

load :: FilePath -> IO (Maybe [Channel])
load fp = runMaybeT $ do
  lift $ putStrLn "Loading users..."
  users <- MaybeT (readUsers fp)

  lift $ putStrLn "Loading channels..."
  chans <- MaybeT (readChannels fp)

  lift $ putStrLn "Loading channel messages.."
  chans' <- MaybeT (readMessages fp $ head chans)

  return chans

readUsers :: FilePath -> IO (Maybe [User])
readUsers fp = parseFile $ fp ++ "/users.json"

readChannels :: FilePath -> IO (Maybe [Channel])
readChannels fp = parseFile $ fp ++ "/channels.json"

readMessages :: FilePath -> Channel -> IO (Maybe [ChatMessage])
readMessages fp c@(Channel id name _) = do
  let bpath = fp ++ "/" ++ name ++ "/"

  dirs <- getDirectoryContents bpath

  let jsonFiles = map ((++) bpath) $ drop 3 dirs

  -- todo: parse, concat
  putStrLn $ show jsonFiles

  return Nothing

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fp = do
  uf <- BS.readFile fp
  return $ decode uf