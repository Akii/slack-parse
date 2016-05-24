module Handler.Channel
  ( getIndexR
  , getChannelR
  ) where

import Import

import Slack

getIndexR :: Handler Html
getIndexR = render Nothing

getChannelR :: ChannelId -> Handler Html
getChannelR cId = do
  app <- getYesod
  render $ lookupChannel cId app

render :: Maybe Channel -> Handler Html
render sel =
  do
    app <- getYesod
    channels <- return $ getChannels app

    case channels of
      Just xs -> renderList xs sel
      Nothing -> renderError

  where
    renderList chans selChan = defaultLayout $ do
                                 setTitle "Slack Archive"
                                 $(widgetFile "homepage")

    renderError = defaultLayout $ do
                    setTitle "Error loading Slack Archive"
                    $(widgetFile "archive-error")

getChannels :: App -> Maybe [Channel]
getChannels a = do
  arch <- slackArchive a
  return $ toList . channels $ arch

lookupChannel :: ChannelId -> App -> Maybe Channel
lookupChannel cId app = do
  arch <- slackArchive app
  chan <- getChannel arch cId
  return chan