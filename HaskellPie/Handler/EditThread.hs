module Handler.EditThread where

import Import
import Helper
import CustomForms (threadMForm)
import Widgets (threadWidget, postWidget, accountLinksW)

getEditThreadR :: ThreadId -> Handler Html
getEditThreadR tid = do
    (thread, isMod) <- runDB $ do
        t <- get404 tid
        isMod <- isModeratorBySession
        return (t, isMod)
    auth <- getThreadPermissions thread
    case (auth || isMod) of
        True -> do
            equation <- liftIO $ createMathEq
            (widget, enctype) <- generateFormPost $ threadMForm equation "Update thread" (Just $ threadTitle thread) (Just $ threadContent thread)
            setSession "captcha" (eqResult equation)
            let headline = threadTitle thread
                leftWidget = threadWidget isMod tid thread
                rightWidget = postWidget enctype widget
            defaultLayout $(widgetFile "left-right-layout")
        (_)  -> redirectUltDest HomeR

postEditThreadR :: ThreadId -> Handler Html
postEditThreadR tid = do
    captcha <- getCaptchaBySession
    equation <- liftIO $ createMathEq
    setSession "captcha" (eqResult equation)
    (thread, isMod) <- runDB $ do
        t <- get404 tid
        isMod <- isModeratorBySession
        return (t, isMod)
    auth <- getThreadPermissions thread
    case (auth || isMod) of
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
                            let headline = threadTitle thread
                                leftWidget   = threadWidget isMod tid thread
                                rightWidget  = [whamlet|<span .simpleBlack> Sorry, the captcha is wrong|] >> postWidget enctype widget
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
        (_)   -> redirectUltDest HomeR
