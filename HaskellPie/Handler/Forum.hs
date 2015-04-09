module Handler.Forum where

import Import
import Widgets (accountLinksW, postWidget, threadListWidget)
import CustomForms (lengthTextField)
import Helper (spacesToMinus, getPersonBySession)
import Yesod.Form.Nic (nicHtmlField)


getForumR :: Handler Html
getForumR = do
    allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]
    (widget, enctype) <- generateFormPost $ threadMForm
    let headline = "Forum" :: Text
    let leftWidget = threadListWidget allThreads
    let rightWidget = postWidget enctype widget
    defaultLayout $(widgetFile "forum")

postForumR :: Handler Html
postForumR = do
    ((result, widget), enctype) <- runFormPost $ threadMForm
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
                    defaultLayout $(widgetFile "forum")
        (FormFailure (err:_))           -> do
            allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]
            let headline = "Forum" :: Text
            let leftWidget = threadListWidget allThreads
            let rightWidget = [whamlet|<span .simpleBlack> Error: #{err} |] >> postWidget enctype widget
            defaultLayout $(widgetFile "forum")
        (_)                             -> do
            allThreads <- runDB $ selectList [] [Desc ThreadLastUpdate]
            let headline = "Forum" :: Text
            let leftWidget = threadListWidget allThreads
            let rightWidget = [whamlet|<span .simpleBlack> Error: Something went wrong, please try again |] >> postWidget enctype widget
            defaultLayout $(widgetFile "forum")

threadMForm :: Form (Maybe Person -> Thread)
threadMForm token = do
    time <- liftIO $ getCurrentTime
    (titleResult, titleView) <- mreq (lengthTextField MsgSubjectError) "" Nothing
    (contentResult, contentView) <- mreq nicHtmlField "" Nothing
    let thread = Thread <$> titleResult <*> contentResult <*> (pure Nothing) <*> (pure time) <*> (pure time)
        widget = [whamlet|
            #{token}
                ^{fvInput titleView}
                ^{fvInput contentView}
                <input type=submit value="Create thread">
            <br>
            <pre> Edit HTML -> &lt;pre&gt; Markup code goes here! &lt;/pre&gt; </pre>
            <label> <a href=http://www.simplehtmlguide.com/text.php style="margin:2px;"> Some how-to use HTML tags </a>
            <label> <a href=lpaste.net> For larger files </a>
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