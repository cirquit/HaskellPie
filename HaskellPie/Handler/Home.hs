module Handler.Home where

import Import
import Widgets (accountLinksW)

getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "homepage")