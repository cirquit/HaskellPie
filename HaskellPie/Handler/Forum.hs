module Handler.Forum where

import Import
import Widgets (accountLinksW, postWidget)
import CustomForms (lengthTextField)
import Helper (spacesToMinus)
import Yesod.Form.Nic (nicHtmlField)


getForumR :: Handler Html
getForumR = do
    allThreads <- runDB $ selectList [] [Desc ThreadTime]
    (widget, enctype) <- generateFormPost $ threadMForm
    let headline = "Forum" :: Text
    let leftWidget = [whamlet|
        <ul>
            $forall (Entity id thread) <- allThreads
                <li> <a href=@{ThreadR $ spacesToMinus $ threadTitle thread} style="margin:10px;"> <label> #{threadTitle thread} </label> </a>
                  |]
    let rightWidget = postWidget enctype widget
    defaultLayout $(widgetFile "forum")

postForumR :: Handler Html
postForumR = do
    ((result, widget), enctype) <- runFormPost $ threadMForm
    mnick <- lookupSession "_ID"
    user <- case mnick of
                   (Just nick) -> do
                       mperson <- runDB $ getBy $ UniqueNick nick
                       case mperson of
                           (Just (Entity _ person)) -> return $ Just person
                           (_)                      -> return $ Nothing
                   (_)         -> return $ Nothing
    case result of
        (FormSuccess threadWithoutUser) -> do
            let thread = threadWithoutUser user
            (mtitle, allThreads) <- runDB $ do
               mtitle <- getBy $ UniqueTitle $ threadTitle thread
               allThreads <- selectList [] [Desc ThreadTime]
               return (mtitle, allThreads)
            case mtitle of
                Nothing -> do
                    (_) <- runDB $ insert thread
                    redirect $ ThreadR (spacesToMinus $ threadTitle thread)
                (_)     -> do
                    let headline = "Forum" :: Text
                    let leftWidget = [whamlet|
                        <ul>
                            $forall (Entity id thread) <- allThreads
                                <li> <a href=@{ThreadR $ spacesToMinus $ threadTitle thread} style="margin:10px;"> <label> #{threadTitle thread} </label> </a>
                        <span .simpleBlack> End of all threads
                        <span .simpleBlack> Error: There is already a thread with this title
                                  |]
                    let rightWidget = postWidget enctype widget
                    defaultLayout $(widgetFile "forum")
        (FormFailure (err:_))           -> do
            allThreads <- runDB $ selectList [] [Desc ThreadTime]
            let headline = "Forum" :: Text
            let leftWidget = [whamlet|
                <ul>
                    $forall (Entity id thread) <- allThreads
                        <li> <a href=@{ThreadR $ spacesToMinus $ threadTitle thread} style="margin:10px;"> <label> #{threadTitle thread} </label> </a>
                             |]
            let rightWidget = [whamlet|<span .simpleBlack> Error: #{err} |] >> postWidget enctype widget
            defaultLayout $(widgetFile "forum")
        (_)                             -> do
            allThreads <- runDB $ selectList [] [Desc ThreadTime]
            let headline = "Forum" :: Text
            let leftWidget = [whamlet|
                <ul>
                    $forall (Entity id thread) <- allThreads
                        <li> <a href=@{ThreadR $ spacesToMinus $ threadTitle thread} style="margin:10px;"> <label> #{threadTitle thread} </label> </a>
                             |]
            let rightWidget = [whamlet|<span .simpleBlack> Error: Something went wrong, please try again |] >> postWidget enctype widget
            defaultLayout $(widgetFile "forum")

threadMForm :: Form (Maybe Person -> Thread)
threadMForm token = do
    time <- liftIO $ getCurrentTime
    (titleResult, titleView) <- mreq (lengthTextField MsgSubjectError) "" Nothing
    (contentResult, contentView) <- mreq nicHtmlField "" Nothing
    let thread = Thread <$> titleResult <*> contentResult <*> (pure Nothing) <*> (pure time)
        widget = [whamlet|
            #{token}
                ^{fvInput titleView}
                ^{fvInput contentView}
                <input type=submit value="Create thread">
                 |] >> toWidget [lucius|
                       ##{fvId contentView} {
                           width:100%;
                           height:200px;
                       }
                       ##{fvId titleView} {
                          width:100%;
                       }
                                |]
    return (thread, widget)