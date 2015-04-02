module Handler.Forum where

import Import
import Widgets (accountLinksW)

getForumR :: Handler Html
getForumR = defaultLayout $(widgetFile "forum")
