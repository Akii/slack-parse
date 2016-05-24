module Slack.Internal.Types.User
  ( UserId
  , User(..)
  ) where

import Data.Aeson

newtype UserId = UserId String deriving (Show, Eq, Ord, FromJSON)

data User = User { uId   :: UserId
                 , nick :: String
                 , name :: Maybe String
                 } deriving (Show)


instance FromJSON User where
  parseJSON = withObject "user with realname" $ \o -> do
    uId        <- o .: "id"
    uNick      <- o .: "name"
    realName   <- o .: "real_name"

    case realName of
      "" -> return $ User uId uNick Nothing
      _  -> return $ User uId uNick (Just realName)