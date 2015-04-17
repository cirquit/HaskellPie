module Handler.Thread where

import Authentification (isModeratorBySession, getValidNickBySession)
import Captcha
import CustomForms (postMForm)
import Helper (minusToSpaces, updatePosts)
import Import
import Widgets (accountLinksW, postWidget, threadWidget)

getThreadR :: Text -> Handler Html
getThreadR title = do

    -- captcha
    equation <- liftIO $ createMathEq
    setSession "captcha" (eqResult equation)

    -- form
    (widget, enctype) <- generateFormPost $ postMForm equation "Submit post" Nothing

    -- db
    (Entity tid thread, isMod) <- runDB $ do
        ethread <- getBy404 $ UniqueTitle $ minusToSpaces title
        isMod   <- isModeratorBySession
        return (ethread, isMod)

    -- widgets

    setUltDestCurrent
    let headline = threadTitle thread
        leftWidget = threadWidget isMod tid thread
        rightWidget = postWidget enctype widget
    defaultLayout $(widgetFile "left-right-layout")

postThreadR :: Text -> Handler Html
postThreadR title = do

    -- captcha
    equation <- liftIO $ createMathEq
    captcha <- getCaptchaBySession
    setSession "captcha" $ eqResult equation

    -- db
    ((Entity tid thread), isMod, nick) <- runDB $ do
        entity <- getBy404 $ UniqueTitle $ minusToSpaces title
        isMod   <- isModeratorBySession
        nick    <- getValidNickBySession
        return (entity, isMod, nick)

    -- form
    ((result, widget), enctype)  <- runFormPost $ postMForm equation "Submit post" Nothing
    (newWidget, newEnctype) <- generateFormPost $ postMForm equation "Submit post" Nothing
    case result of
        (FormSuccess mpost)   -> do
            let post = mpost nick
            case (postCaptcha post) == captcha of
                True -> do
                    (Entity newTid newThread) <- runDB $ do
                        (_) <- update tid [ThreadPosts =. (updatePosts (threadPosts thread) post), ThreadLastUpdate =. (postTime post)]
                        entity <- getBy404 $ UniqueTitle $ minusToSpaces title
                        return entity
                    let headline    = threadTitle newThread
                        leftWidget  = threadWidget isMod newTid newThread
                        rightWidget = postWidget newEnctype newWidget
                    defaultLayout $(widgetFile "left-right-layout")
                (_)  -> do
                    let headline    = threadTitle thread
                        leftWidget  = threadWidget isMod tid thread
                        rightWidget = [whamlet|<span .simpleBlack> Sorry, the captcha is wrong|] >> postWidget enctype widget
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