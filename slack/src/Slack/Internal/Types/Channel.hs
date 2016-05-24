module Slack.Internal.Types.Channel
  ( ChannelId(..)
  , Channel(..)
  ) where

import GHC.Generics
import Data.Aeson
import Control.Applicative (empty)

import Slack.Internal.Types.ChatMessage (ChatMessage)

newtype ChannelId = ChannelId String deriving (Show, Eq, Ord, Read, FromJSON)

data Channel = Channel { cId    :: ChannelId
                       , title  :: String
                       , msgs   :: [ChatMessage]
                       } deriving (Show)


instance FromJSON Channel where
  parseJSON (Object v) = Channel
    <$> v .:  "id"
    <*> v .:  "name"
    <*> pure []

  parseJSON _ = empty