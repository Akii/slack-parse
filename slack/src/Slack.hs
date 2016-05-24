module Slack
  ( ChannelId(..)
  , load
  , SlackArchive(..)
  , countNumberOfChatMessages
  ) where

import Slack.Internal.Reading (load)
import Slack.Internal.Types