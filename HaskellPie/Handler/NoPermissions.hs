module Handler.NoPermissions where

import Import
import Widgets  (accountLinksW)

getNoPermissionsR :: Handler Html
getNoPermissionsR = do
  let headline = "You don't have enough permissions to do this" :: Text
      leftWidget =  [whamlet|<span>|]
      rightWidget = [whamlet|<span>|]
  defaultLayout $(widgetFile "left-right-layout")
