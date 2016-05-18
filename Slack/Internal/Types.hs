module Slack.Internal.Types where

import Prelude hiding (id)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Control.Applicative (empty)

newtype UserId = UserId String deriving (Show, Eq, FromJSON)

-- todo: could have a profile
data User = User { id   :: UserId
                 , nick :: String
                 , name :: Maybe String
                 }
          | Unknown UserId deriving (Show)

newtype ChannelId = ChannelId String deriving (Show, FromJSON)

data Channel = Channel ChannelId String [ChatMessage] deriving (Show)

data MessageType = Message | UnknownMsgType Text deriving (Show)

data MassgeSubType = FileShare
                    | ChannelJoin
                    | ChannelPurpose
                    | ChannelArchive
                    | UnknownSubMsgType Text
                    deriving (Show)

-- todo: date time parsing, sub type dependent data
-- todo: check why I can't write `data Message = Message`
-- todo: should assign channelId or put into channel
data ChatMessage = ChatMessage UserId MessageType (Maybe MassgeSubType) Text String deriving (Show)

-- FromJSON instances
instance FromJSON User where
  parseJSON = withObject "user with realname" $ \o -> do
    uId        <- o .: "id"
    uNick      <- o .: "name"
    realName   <- o .: "real_name"

    case realName of
      "" -> return $ User uId uNick Nothing
      _  -> return $ User uId uNick (Just realName)

instance FromJSON Channel where
  parseJSON (Object v) = Channel
    <$> v .:  "id"
    <*> v .:  "name"
    <*> pure []

  parseJSON _ = empty

instance FromJSON MessageType where
  parseJSON (String s) = case s of
    "message" -> return $ Message
    msg       -> return $ UnknownMsgType msg

  parseJSON _ = fail "Can only convert a string to a channel type"

instance FromJSON MassgeSubType where
  parseJSON (String s) = case s of
    "file_share"        -> return $ FileShare
    "channel_join"      -> return $ ChannelJoin
    "channel_purpose"   -> return $ ChannelPurpose
    "channel_archive"   -> return $ ChannelArchive
    msg                 -> return $ UnknownSubMsgType msg

  parseJSON _ = fail "Can only convert a string to a channel subtype"

instance FromJSON ChatMessage where
  parseJSON (Object v) = ChatMessage
    <$> v .:  "user"
    <*> v .:  "type"
    <*> v .:? "subtype"
    <*> v .:  "text"
    <*> v .:  "ts"

  parseJSON _ = empty