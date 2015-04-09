module Handler.Contact where

import Import
import Widgets (accountLinksW)

getContactR :: Handler Html
getContactR = do
    let content = [whamlet|
        <div style="text-align:center; margin: 30px 0px 0px 0px">
            <span #contact> Write anything concerning this site to this email:
            <br>
            <span #contact> alex.isenko@googlemail.com
                  |]
    defaultLayout $(widgetFile "homepage")