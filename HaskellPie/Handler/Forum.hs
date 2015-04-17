module Handler.Forum where

import Import
import Widgets (accountLinksW, threadListWidget)

getForumR :: Handler Html
getForumR = do
    -- db
    allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]

    -- widgets

    setUltDestCurrent
    let headline = "Forum" :: Text
    let midWidget = threadListWidget allThreads 75 >> [whamlet|
        <form action=@{CreateThreadR}>
            <input type="submit" value="Create new thread">
                                                   |]
    defaultLayout $(widgetFile "mid-layout")