module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  arch <- fmap slackArchive getYesod

  case arch of
    Just a  -> renderHome
    Nothing -> renderError

renderHome =
  defaultLayout $ do
    setTitle "Slack Archive"
    $(widgetFile "homepage")

renderError =
  defaultLayout $ do
    setTitle "Error"
    $(widgetFile "archive-error")