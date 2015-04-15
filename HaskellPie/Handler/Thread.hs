module Handler.Thread where

import Import
import Widgets (accountLinksW, postWidget, threadWidget)
import Helper (minusToSpaces, addPost, getPersonBySession, isModeratorBySession)
import CustomForms (postMForm)

getThreadR :: Text -> Handler Html
getThreadR text = do
    setUltDestCurrent
    (widget, enctype) <- generateFormPost $ postMForm "Submit post" Nothing
    let completeTitle = minusToSpaces text
    (Entity tid thread, isMod) <- runDB $ do
        ethread <- getBy404 $ UniqueTitle completeTitle
        isMod   <- isModeratorBySession
        return (ethread, isMod)
    let headline = threadTitle thread
        leftWidget = threadWidget isMod tid thread
        rightWidget = postWidget enctype widget
    defaultLayout $(widgetFile "left-right-layout")

postThreadR :: Text -> Handler Html
postThreadR text = do
    let completeTitle = minusToSpaces text
    ((Entity tid thread), isMod) <- runDB $ do
        ethread <- getBy404 $ UniqueTitle completeTitle
        isMod   <- isModeratorBySession
        return (ethread, isMod)
    ((result, widget), enctype) <- runFormPost $ postMForm "Submit post" Nothing
    (newWidget, newEnctype) <- generateFormPost $ postMForm "Submit post" Nothing
    case result of
        (FormSuccess mpost)   -> do
            user <- runDB $ getPersonBySession
            let post = mpost user
            ((Entity newTid newThread)) <- runDB $ do
                (_) <- update tid [ThreadPosts =. (addPost (threadPosts thread) post), ThreadLastUpdate =. (postTime post)]
                entity <- getBy404 $ UniqueTitle completeTitle
                return (entity)
            let headline    = threadTitle newThread
                leftWidget  = threadWidget isMod newTid newThread
                rightWidget = postWidget newEnctype newWidget
            defaultLayout $(widgetFile "left-right-layout")
        (FormFailure (err:_)) -> do
            let headline    = threadTitle thread
                leftWidget  = threadWidget isMod tid thread
                rightWidget = [whamlet|<span .simpleBlack> #{err}|] >> postWidget enctype widget
            defaultLayout $(widgetFile "left-right-layout")
        (_)                   -> do
            let headline    = threadTitle thread
                leftWidget  = threadWidget isMod tid thread
                rightWidget = [whamlet|<span .simpleBlack> Something went wrong, please try again|] >> postWidget enctype widget
            defaultLayout $(widgetFile "left-right-layout")