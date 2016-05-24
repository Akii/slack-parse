module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR =
  do
    --    chan <- lookupGetParam "channel"
    --
    --    case tag_m of
    --        Nothing  -> publishedPosts
    --        Just tag -> publishedPostsByTag tag

    arch <- slackArchive <$> getYesod

    case arch of
      Just a  -> renderHome
      Nothing -> renderError
  where
    renderHome = defaultLayout $ do
        setTitle "Slack Archive"
        $(widgetFile "homepage")

    renderError = defaultLayout $ do
        setTitle "Error"
        $(widgetFile "archive-error")