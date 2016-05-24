module Slack.Internal.Types.ChatMessage
  ( MessageId
  , ChatMessage(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Control.Applicative (empty)

import Slack.Internal.Types.User (UserId)

newtype MessageId = MessageId String deriving (Show, Eq, Ord, FromJSON)

-- todo: date time parsing, sub type dependent data
data ChatMessage = ChatMessage MessageId UserId MessageType (Maybe MassgeSubType) Text String deriving (Show)

data MessageType = Message
                 | UnknownMsgType Text
                 deriving (Show)

data MassgeSubType = FileShare
                   | ChannelJoin
                   | ChannelLeave
                   | MeMessage
                   | ChannelPurpose
                   | ChannelArchive
                   | UnknownSubMsgType Text
                   deriving (Show)


instance FromJSON MessageType where
  parseJSON (String s) = case s of
    "message" -> return $ Message
    msg       -> return $ UnknownMsgType msg

  parseJSON _ = fail "Can only convert a string to a channel type"

instance FromJSON MassgeSubType where
  parseJSON (String s) = case s of
    "file_share"        -> return $ FileShare
    "channel_join"      -> return $ ChannelJoin
    "channel_leave"     -> return $ ChannelLeave
    "me_message"        -> return $ MeMessage
    "channel_purpose"   -> return $ ChannelPurpose
    "channel_archive"   -> return $ ChannelArchive
    msg                 -> return $ UnknownSubMsgType msg

  parseJSON _ = fail "Can only convert a string to a channel subtype"

instance FromJSON ChatMessage where
  parseJSON (Object v) = ChatMessage
    <$> v .:  "ts"
    <*> v .:  "user"
    <*> v .:  "type"
    <*> v .:? "subtype"
    <*> v .:  "text"
    <*> v .:  "ts"

  parseJSON _ = empty