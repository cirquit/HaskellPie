module Handler.Tutorials where

import Import
import Widgets (accountLinksW)

getTutorialsR :: Handler Html
getTutorialsR = do
    let content = [whamlet|
        <div style="text-align:center;">
            <br>
            <br>
            <span> Work in progress...
                  |]
    defaultLayout $(widgetFile "tutorials")
