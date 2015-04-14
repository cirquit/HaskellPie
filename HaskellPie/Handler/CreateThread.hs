module Handler.CreateThread where


import Import
import Widgets (accountLinksW, postWidget, threadListWidget)
import CustomForms (threadMForm)
import Helper (spacesToMinus, getPersonBySession)


getCreateThreadR :: Handler Html
getCreateThreadR = do
    (widget, enctype) <- generateFormPost $ threadMForm "Create thread" Nothing Nothing
    let headline = "Create a new thread" :: Text
    let midWidget = postWidget enctype widget
    defaultLayout $(widgetFile "mid-layout")

postCreateThreadR :: Handler Html
postCreateThreadR = do
    ((result, widget), enctype) <- runFormPost $ threadMForm "Create thread" Nothing Nothing
    user <- runDB $ getPersonBySession
    case result of
        (FormSuccess mthread) -> do
            let thread = mthread user
            mtitle <- runDB $ getBy $ UniqueTitle $ threadTitle thread
            case mtitle of
                Nothing -> do
                    (_) <- runDB $ insert thread
                    redirect $ ThreadR (spacesToMinus $ threadTitle thread)
                (_)     -> do
                    let headline = "Forum" :: Text
                    let midWidget = [whamlet|<span .simpleBlack> Error: Sorry, this thread already exists |] >> postWidget enctype widget
                    defaultLayout $(widgetFile "mid-layout")
        (FormFailure (err:_))           -> do
            let headline = "Forum" :: Text
            let midWidget = [whamlet|<span .simpleBlack> Error: #{err} |] >> postWidget enctype widget
            defaultLayout $(widgetFile "mid-layout")
        (_)                             -> do
            let headline = "Forum" :: Text
            let midWidget = [whamlet|<span .simpleBlack> Error: Something went wrong, please try again |] >> postWidget enctype widget
            defaultLayout $(widgetFile "mid-layout")