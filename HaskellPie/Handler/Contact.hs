module Handler.Contact where

import Import
import Widgets (accountLinksW)

getContactR :: Handler Html
getContactR = defaultLayout $(widgetFile "contact")
