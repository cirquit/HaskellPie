module Handler.EditPost where

import Authentification (isModeratorBySession, getPostPermissions)
import Captcha
import CustomForms (postMForm)
import Helper (replacePostByIndex)
import Import
import Widgets (threadWidget, postWidget, accountLinksW)

getEditPostR :: ThreadId -> Int -> Handler Html
getEditPostR tid n = do

    -- db && auth
    (thread, isMod) <- runDB $ do
        t      <- get404 tid
        isMod  <- isModeratorBySession
        return (t, isMod)
    (isAuthor, mpost) <- getPostPermissions thread n

    case ((isAuthor || isMod), mpost) of
        (True, Just post) -> do

            -- captcha
            equation <- liftIO $ createMathEq
            setSession "captcha" (eqResult equation)

            -- form
            (widget, enctype) <- generateFormPost $ postMForm equation "Update post" (Just $ postContent post)

            -- widgets
            let headline = threadTitle thread
                leftWidget = threadWidget isMod tid thread
                rightWidget = postWidget enctype widget
            defaultLayout $(widgetFile "left-right-layout")

        (_,_)                    -> redirectUltDest HomeR


postEditPostR :: ThreadId -> Int -> Handler Html
postEditPostR tid n = do

    -- captcha
    captcha <- getCaptchaBySession
    equation <- liftIO $ createMathEq
    setSession "captcha" (eqResult equation)

    -- db && auth
    (thread, isMod) <- runDB $ do
        t <- get404 tid
        isMod <- isModeratorBySession
        return (t, isMod)
    (isAuthor, oldPost) <- getPostPermissions thread n

    -- widgets
    let headline    = threadTitle thread
        leftWidget  = threadWidget isMod tid thread

    -- form
    case ((isAuthor || isMod), oldPost) of
        (True, Just post)   -> do
            ((result, widget),enctype) <- runFormPost $ postMForm equation "Update post" (Just $ postContent post)
            case result of
                (FormSuccess mpost) -> do
                    let newPost = mpost $ postCreator post
                    case (postCaptcha newPost) == captcha of
                        True -> do
                            (_) <- runDB $ replace tid (replacePostByIndex thread newPost n)
                            redirectUltDest HomeR
                        (_) -> do
                            let rightWidget = [whamlet|<span .simpleBlack> Sorry, the captcha is wrong|] >> postWidget enctype widget
                            defaultLayout $(widgetFile "left-right-layout")
                (FormFailure (err:_)) -> do
                    let rightWidget = [whamlet|<span .simpleBlack> #{err}|] >> postWidget enctype widget
                    defaultLayout $(widgetFile "left-right-layout")
                (_)                   -> do
                    let rightWidget = [whamlet|<span .simpleBlack> Something went wrong, please try again|] >> postWidget enctype widget
                    defaultLayout $(widgetFile "left-right-layout")
        (_, _)                      -> redirectUltDest HomeR