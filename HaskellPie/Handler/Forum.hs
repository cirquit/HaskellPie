module Handler.Forum where

import Import
import Widgets (accountLinksW, threadListWidget)

getForumR :: Handler Html
getForumR = do
    allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]
    let headline = "Forum" :: Text
    let midWidget = threadListWidget allThreads 75 >> [whamlet|
        <form action=@{CreateThreadR}>
            <input type="submit" value="Create new thread">
                                                   |]
    defaultLayout $(widgetFile "mid-layout")