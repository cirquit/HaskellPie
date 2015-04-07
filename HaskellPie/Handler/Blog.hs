module Handler.Blog where

import Import
import Widgets (accountLinksW)

getBlogR :: Handler Html
getBlogR = do
    let content = [whamlet|<span>|]
    defaultLayout $(widgetFile "homepage")