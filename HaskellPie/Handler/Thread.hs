module Handler.Thread where

import Import
import Widgets (accountLinksW, postWidget, threadWidget)
import Helper (minusToSpaces, addPost)
import CustomForms (postMForm)

getThreadR :: Text -> Handler Html
getThreadR text = do
    setUltDestCurrent
    (widget, enctype) <- generateFormPost $ postMForm "Submit post" Nothing
    let completeTitle = minusToSpaces text
    (Entity tid thread) <- runDB $ getBy404 $ UniqueTitle completeTitle
    let headline = threadTitle thread
        leftWidget = threadWidget tid thread
        rightWidget = postWidget enctype widget
    defaultLayout $(widgetFile "forum")

postThreadR :: Text -> Handler Html
postThreadR text = do
    let completeTitle = minusToSpaces text
    mnick <- lookupSession "_ID"
    (Entity tid thread) <- runDB $ getBy404 $ UniqueTitle completeTitle
    user <- case mnick of
                   (Just nick) -> do
                       mperson <- runDB $ getBy $ UniqueNick nick
                       case mperson of
                          (Just (Entity _ person)) -> return $ Just person
                          (_)               -> return $ Nothing
                   (_)           -> return $ Nothing
    let oldPosts = threadPosts thread
    ((result, widget), enctype) <- runFormPost $ postMForm "Submit post" Nothing
    (nwidget, nenctype)         <- generateFormPost $ postMForm "Submit post" Nothing
    case result of
        (FormSuccess mpost)   -> do
            let post = mpost user
            (Entity ntid newThread) <- runDB $ do
                (_) <- update tid [ThreadPosts =. (addPost oldPosts post), ThreadLastUpdate =. (postTime post)]
                entity <-getBy404 $ UniqueTitle completeTitle
                return entity
            let headline    = threadTitle thread
                leftWidget  = threadWidget ntid newThread
                rightWidget = postWidget nenctype nwidget
            defaultLayout $(widgetFile "forum")
        (FormFailure (err:_)) -> do
            let headline    = threadTitle thread
                leftWidget  = threadWidget tid thread
                rightWidget = [whamlet|<span .simpleBlack> #{err}|] >> postWidget enctype widget
            defaultLayout $(widgetFile "forum")
        (_)                   -> do
            let headline    = threadTitle thread
                leftWidget  = threadWidget tid thread
                rightWidget = [whamlet|<span .simpleBlack> Something went wrong, please try again|] >> postWidget enctype widget
            defaultLayout $(widgetFile "forum")