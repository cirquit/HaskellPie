module Handler.Contact where

import Import
import Widgets (accountLinksW)

getContactR :: Handler Html
getContactR = do
    let content = [whamlet|
        <div style="text-align:center;">
            <br>
            <br>
            <span> Write anything concerning this site to this email:
            <br>
            <span> alex.isenko@googlemail.com
                  |]
    defaultLayout $(widgetFile "contact")