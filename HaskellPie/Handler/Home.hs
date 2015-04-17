module Handler.Home where

import Import
import Widgets (accountLinksW)

getHomeR :: Handler Html
getHomeR = do
    let content = [whamlet|
        <div style="text-align:center;">
            <br>
            <br>
            <span> Work in progress...
                  |]
    defaultLayout $(widgetFile "home")