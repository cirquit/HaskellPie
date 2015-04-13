module Handler.Error where

import Widgets (accountLinksW)
import Import

getErrorR :: Handler Html
getErrorR = do
    let headline = "Sorry, this URL does not exist" :: Text
        leftWidget  = [whamlet| <span> |]
        rightWidget = [whamlet| <span> |]
    defaultLayout $(widgetFile "left-right-layout")
