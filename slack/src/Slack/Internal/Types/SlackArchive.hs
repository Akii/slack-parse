module Slack.Internal.Types.SlackArchive
  ( SlackArchive(..)
  , mkSlackArchive
  , countNumberOfChatMessages
  ) where

import Prelude hiding (foldr)
import Data.Map (fromList, Map, foldr, union)

import           Slack.Internal.Types.User    (UserId, User(..))
import           Slack.Internal.Types.Channel (ChannelId, Channel(..))
import qualified Slack.Internal.Types.Channel as C

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

merge :: SlackArchive -> SlackArchive -> SlackArchive
merge sa1 sa2 = SlackArchive {
    users = union (users sa1) (users sa2)
  , channels = channels sa1 -- todo: merge channels
}