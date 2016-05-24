module Slack.Internal.Types.SlackArchive
  ( SlackArchive(..)
  , mkSlackArchive
  , countNumberOfChatMessages
  , getUser
  , getChannel
  , mergeArchive
  ) where

import Prelude hiding (foldr, lookup)
import Data.Map (fromList, Map, foldr, union, lookup)

import           Slack.Internal.Types.User    (UserId, User(..))
import           Slack.Internal.Types.Channel (ChannelId, Channel(..))

data SlackArchive = SlackArchive { users    :: Map UserId User
                                 , channels :: Map ChannelId Channel
                                 } deriving (Show)

mkSlackArchive :: [User] -> [Channel] -> SlackArchive
mkSlackArchive us cs =
  let uMap = fromList $ map (\u -> (uId u,u)) us
      cMap = fromList $ map (\c -> (cId c,c)) cs
  in
    SlackArchive uMap cMap

countNumberOfChatMessages :: SlackArchive -> Int
countNumberOfChatMessages (SlackArchive _ chans) = foldr (\a b -> b + (length $ msgs a)) 0 chans

getUser :: SlackArchive -> UserId -> Maybe User
getUser arch id = lookup id (users arch)

getChannel :: SlackArchive -> ChannelId -> Maybe Channel
getChannel arch id = lookup id (channels arch)

mergeArchive :: SlackArchive -> SlackArchive -> SlackArchive
mergeArchive sa1 sa2 = SlackArchive {
    users = union (users sa1) (users sa2)
  , channels = channels sa1 -- todo: merge channels
}