module Handler.Forum where

import Import
import Widgets (accountLinksW, postWidget, threadListWidget)
import CustomForms (threadMForm)
import Helper (spacesToMinus, getPersonBySession)


getForumR :: Handler Html
getForumR = do
    allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]
    (widget, enctype) <- generateFormPost $ threadMForm "Create thread" Nothing Nothing
    let headline = "Forum" :: Text
    let leftWidget = threadListWidget allThreads
    let rightWidget = postWidget enctype widget
    defaultLayout $(widgetFile "left-right-layout")

postForumR :: Handler Html
postForumR = do
    ((result, widget), enctype) <- runFormPost $ threadMForm "Create thread" Nothing Nothing
    user <- runDB $ getPersonBySession
    case result of
        (FormSuccess mthread) -> do
            let thread = mthread user
            (mtitle, allThreads) <- runDB $ do
               mtitle <- getBy $ UniqueTitle $ threadTitle thread
               allThreads <- selectList [] [Desc ThreadLastUpdate]
               return (mtitle, allThreads)
            case mtitle of
                Nothing -> do
                    (_) <- runDB $ insert thread
                    redirect $ ThreadR (spacesToMinus $ threadTitle thread)
                (_)     -> do
                    let headline = "Forum" :: Text
                    let leftWidget = threadListWidget allThreads
                    let rightWidget = [whamlet|<span .simpleBlack> Error: Sorry, this thread already exists |] >> postWidget enctype widget
                    defaultLayout $(widgetFile "left-right-layout")
        (FormFailure (err:_))           -> do
            allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]
            let headline = "Forum" :: Text
            let leftWidget = threadListWidget allThreads
            let rightWidget = [whamlet|<span .simpleBlack> Error: #{err} |] >> postWidget enctype widget
            defaultLayout $(widgetFile "left-right-layout")
        (_)                             -> do
            allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]
            let headline = "Forum" :: Text
            let leftWidget = threadListWidget allThreads
            let rightWidget = [whamlet|<span .simpleBlack> Error: Something went wrong, please try again |] >> postWidget enctype widget
            defaultLayout $(widgetFile "left-right-layout")