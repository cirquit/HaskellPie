module Handler.Forum where

import Import
import Widgets (accountLinksW, threadListWidget)

getForumR :: Handler Html
getForumR = do

    -- db
    allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]

    -- widgets
    setUltDestCurrent
    let midWidget = threadListWidget allThreads 75
    defaultLayout $(widgetFile "forum")