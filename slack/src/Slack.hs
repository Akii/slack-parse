module Slack
  ( ChannelId(..)
  , Channel(..)
  , MessageId(..)
  , ChatMessage(..)
  , SlackArchive(..)
  , load
  , getUser
  , getChannel
  , countNumberOfChatMessages
  , mergeArchive
  ) where

import Slack.Internal.Reading (load)
import Slack.Internal.Types