module Handler.Forum where

import Import
import Widgets (accountLinksW)

getForumR :: Handler Html
getForumR = do
    let content = [whamlet|<span>|]
    defaultLayout $(widgetFile "homepage")
