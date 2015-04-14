module Handler.Forum where

import Import
import Widgets (accountLinksW, threadListWidget)
--import CustomForms (threadMForm)
--import Helper (spacesToMinus, getPersonBySession)


getForumR :: Handler Html
getForumR = do
    allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]
    let headline = "Forum" :: Text
    let midWidget = threadListWidget allThreads 50 >> [whamlet|
        <form action=@{CreateThreadR}>
            <input type="submit" value="Create new thread">
                                                   |]
    defaultLayout $(widgetFile "mid-layout")