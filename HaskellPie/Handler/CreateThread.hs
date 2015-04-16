module Handler.CreateThread where


import Import
import Widgets (accountLinksW, postWidget)
import CustomForms (threadMForm)
import Captcha (createMathEq, getCaptchaBySession)
import Authentification (getValidNickBySession)

getCreateThreadR :: Handler Html
getCreateThreadR = do
    -- captcha
    equation <- liftIO $ createMathEq
    setSession "captcha" $ eqResult equation

    -- form
    (widget, enctype) <- generateFormPost $ threadMForm equation "Create thread" Nothing Nothing

    -- widgets
    let headline = "Create a new thread" :: Text
        midWidget = postWidget enctype widget
    defaultLayout $(widgetFile "mid-layout")


postCreateThreadR :: Handler Html
postCreateThreadR = do
    -- captcha
    equation <- liftIO $ createMathEq
    captcha <- getCaptchaBySession
    setSession "captcha" $ eqResult equation

    -- widgets
    let headline = "Create a new thread" :: Text

    -- form
    ((result, widget), enctype) <- runFormPost $ threadMForm equation "Create thread" Nothing Nothing
    case result of
        (FormSuccess mthread) -> do
            mtitle <- runDB $ do
                    mtitle <- getBy $ UniqueTitle $ threadTitle $ mthread Nothing
                    user   <- getValidNickBySession
                return (mtitle, user)
            case (mtitle, (threadCaptcha thread == captcha)) of
                (Nothing, True) -> do
                    (_) <- runDB $ insert thread
                    redirect $ ThreadR (spacesToMinus $ threadTitle thread)
                ((Just _), _)     -> do
                    let midWidget = [whamlet|<span .simpleBlack> Error: Sorry, this thread already exists |] >> postWidget enctype widget
                    defaultLayout $(widgetFile "mid-layout")
                (_, _)     -> do
                    let midWidget = [whamlet|<span .simpleBlack> Error: Sorry, the captcha is wrong |] >> postWidget enctype widget
                    defaultLayout $(widgetFile "mid-layout")
        (FormFailure (err:_))           -> do
            let midWidget = [whamlet|<span .simpleBlack> Error: #{err} |] >> postWidget enctype widget
            defaultLayout $(widgetFile "mid-layout")
        (_)                             -> do
            let midWidget = [whamlet|<span .simpleBlack> Error: Something went wrong, please try again |] >> postWidget enctype widget
            defaultLayout $(widgetFile "mid-layout")