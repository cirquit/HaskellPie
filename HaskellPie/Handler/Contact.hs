module Handler.Contact where

import Import
import Widgets (accountLinksW)

getContactR :: Handler Html
getContactR = do
    let content = [whamlet|<span>|]
    defaultLayout $(widgetFile "homepage")