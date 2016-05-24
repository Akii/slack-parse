module Handler.Channel where

import Import

import Slack (ChannelId)

getChannelR :: ChannelId -> Handler Html
getChannelR s = defaultLayout $ do
        setTitle $ fromString $ (show s) ++ "wat"
        $(widgetFile "homepage")