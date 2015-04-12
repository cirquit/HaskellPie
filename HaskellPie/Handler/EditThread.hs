module Handler.EditThread where

import Import
import Helper (getThreadPermissions, spacesToMinus)
import CustomForms (threadMForm)
import Widgets (threadWidget, postWidget, accountLinksW)

getEditThreadR :: ThreadId -> Handler Html
getEditThreadR tid = do
    thread <- runDB $ get404 tid
    auth <- getThreadPermissions thread
    case auth of
        True -> do
            (widget, enctype) <- generateFormPost $ threadMForm "Update thread" (Just $ threadTitle thread) (Just $ threadContent thread)
            let headline = threadTitle thread
                leftWidget = threadWidget tid thread
                rightWidget = postWidget enctype widget
            defaultLayout $(widgetFile "forum")
        (_)  -> redirectUltDest HomeR

postEditThreadR :: ThreadId -> Handler Html
postEditThreadR tid = do
    thread <- runDB $ get404 tid
    auth <- getThreadPermissions thread
    case auth of
        True  -> do
            ((result, widget),enctype)<- runFormPost $ threadMForm "Update thread" (Just $ threadTitle thread) (Just $ threadContent thread)
            case result of
                (FormSuccess mthread)   -> do
                    let newThread = mthread (threadCreator thread)
                    runDB $ replace tid $ newThread {threadPosts = (threadPosts thread), threadCreator = (threadCreator thread)}
                    redirect $ ThreadR (spacesToMinus $ threadTitle newThread)
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
        (_)   -> redirectUltDest HomeR
