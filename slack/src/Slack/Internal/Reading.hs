module Slack.Internal.Reading (load) where

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
  users <- readUsers fp
  chans <- readChannels fp

  fmap (mkSlackArchive users) $ mapM (loadChannelMessages fp) chans

readUsers :: FilePath -> MaybeT IO ([User])
readUsers fp = MaybeT . parseFile $ fp ++ "/users.json"

readChannels :: FilePath -> MaybeT IO ([Channel])
readChannels fp = MaybeT . parseFile $ fp ++ "/channels.json"

loadChannelMessages :: FilePath -> Channel -> MaybeT IO (Channel)
loadChannelMessages fp (Channel cId cName _) =
  do
    channelFiles <- lift $ getDirectoryContents basePath
    chatMsgs <- readMessages $ (prefixBasePath . filterJsonFiles) channelFiles basePath

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