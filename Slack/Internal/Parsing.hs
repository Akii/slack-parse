module Slack.Internal.Parsing (load) where

import            Prelude
import            Data.Aeson
import qualified  Data.ByteString.Lazy as BS
import            Control.Monad.Trans.Maybe
import            Control.Monad.Trans.Class (lift)

import Slack.Internal.Types

load :: FilePath -> IO (Maybe [Channel])
load fp = runMaybeT $ do
  lift $ putStrLn "Loading users..."
  users <- MaybeT (readUsers fp)

  lift $ putStrLn "Loading channels..."
  chans <- MaybeT (readChannels fp)

  lift $ putStrLn "Loading channel messages.."
--  map readMessages

  return []

readUsers :: FilePath -> IO (Maybe [User])
readUsers fp = parseFile $ fp ++ "/users.json"

readChannels :: FilePath -> IO (Maybe [Channel])
readChannels fp = parseFile $ fp ++ "/channels.json"

readMessages :: FilePath -> Channel -> IO (Channel)
readMessages = undefined

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fp = do
  uf <- BS.readFile fp
  return $ decode uf