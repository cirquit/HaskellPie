module Handler.Error where

import Widgets (accountLinksW)
import Import

getErrorR :: Handler Html
getErrorR = do
    let content = [whamlet|
        <span .simpleBlack> Sorry, you can't do that
                  |]
    defaultLayout $(widgetFile "homepage")
