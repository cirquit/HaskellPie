module Handler.EditThread where

import Authentification (isModeratorBySession, getThreadPermissions)
import Captcha
import CustomForms (threadMForm)
import Import
import Helper (spacesToMinus)
import Widgets (threadWidget, postWidget, accountLinksW)

getEditThreadR :: ThreadId -> Handler Html
getEditThreadR tid = do

    -- db && auth
    (thread, isMod) <- runDB $ do
        t <- get404 tid
        isMod <- isModeratorBySession
        return (t, isMod)
    isAuthor <- getThreadPermissions thread

    case (isAuthor || isMod) of
        True -> do

            -- captcha
            equation <- liftIO $ createMathEq
            setSession "captcha" (eqResult equation)

            -- form
            (widget, enctype) <- generateFormPost $ threadMForm equation "Update thread" (Just $ threadTitle thread) (Just $ threadContent thread)

            -- widgets
            let headline = threadTitle thread
                leftWidget = threadWidget isMod tid thread
                rightWidget = postWidget enctype widget
            defaultLayout $(widgetFile "left-right-layout")

        (_)  -> redirectUltDest HomeR

postEditThreadR :: ThreadId -> Handler Html
postEditThreadR tid = do

    -- captcha
    captcha <- getCaptchaBySession
    equation <- liftIO $ createMathEq
    setSession "captcha" (eqResult equation)

    -- db & auth
    (thread, isMod) <- runDB $ do
        t <- get404 tid
        isMod <- isModeratorBySession
        return (t, isMod)
    isAuthor <- getThreadPermissions thread

    -- widgets
    let headline = threadTitle thread
        leftWidget   = threadWidget isMod tid thread

    case (isAuthor || isMod) of
        True  -> do
            ((result, widget),enctype)<- runFormPost $ threadMForm equation "Update thread" (Just $ threadTitle thread) (Just $ threadContent thread)
            case result of
                (FormSuccess mthread)   -> do
                    let newThread = mthread (threadCreator thread)
                    case (threadCaptcha newThread) == captcha of
                        True -> do
                            runDB $ replace tid $ newThread {threadPosts = (threadPosts thread), threadCreator = (threadCreator thread)}
                            redirect $ ThreadR (spacesToMinus $ threadTitle newThread)
                        False -> do
                            let rightWidget  = [whamlet|<span .simpleBlack> Sorry, the captcha is wrong|] >> postWidget enctype widget
                            defaultLayout $(widgetFile "left-right-layout")
                (FormFailure (err:_)) -> do
                    let rightWidget = [whamlet|<span .simpleBlack> #{err}|] >> postWidget enctype widget
                    defaultLayout $(widgetFile "left-right-layout")
                (_)                   -> do
                    let rightWidget = [whamlet|<span .simpleBlack> Something went wrong, please try again|] >> postWidget enctype widget
                    defaultLayout $(widgetFile "left-right-layout")
        (_)   -> redirectUltDest HomeR
