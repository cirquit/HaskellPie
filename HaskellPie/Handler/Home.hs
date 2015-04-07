module Handler.Home where

import Import
import Widgets (accountLinksW)

getHomeR :: Handler Html
getHomeR = do
    let content = [whamlet|<span>|]
    defaultLayout $(widgetFile "homepage")