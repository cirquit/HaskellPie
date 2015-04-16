module Handler.EditPost where

import Import
import CustomForms (postMForm)
import Widgets (threadWidget, postWidget, accountLinksW)
import Helper

getEditPostR :: ThreadId -> Int -> Handler Html
getEditPostR tid n = do
    (thread, isMod) <- runDB $ do
        t      <- get404 tid
        isMod  <- isModeratorBySession
        return (t, isMod)
    let mpost = getPostByIndex thread n
    auth <- getPostPermissions thread n
    liftIO $ do
        putStrLn $ ("auth:  " :: Text) ++ (pack (show auth))
        putStrLn $ ("isMod: " :: Text) ++ (pack (show isMod))
        putStrLn $ ("post : " :: Text) ++ (pack (show (maybe "none" (show . postCaptcha) mpost)))
    case ((auth || isMod), mpost) of
        (True, Just post) -> do
            equation <- liftIO $ createMathEq
            setSession "captcha" (eqResult equation)
            (widget, enctype) <- generateFormPost $ postMForm equation "Update post" (Just $ postContent post)
            let headline = threadTitle thread
                leftWidget = threadWidget isMod tid thread
                rightWidget = postWidget enctype widget
            defaultLayout $(widgetFile "left-right-layout")
        (_,_)                    -> redirectUltDest HomeR


postEditPostR :: ThreadId -> Int -> Handler Html
postEditPostR tid n = do
    captcha <- getCaptchaBySession
    equation <- liftIO $ createMathEq
    setSession "captcha" (eqResult equation)
    (thread, isMod) <- runDB $ do
        t <- get404 tid
        isMod <- isModeratorBySession
        return (t, isMod)
    let oldPost = getPostByIndex thread n
    auth <- getPostPermissions thread n
    case ((auth || isMod), oldPost) of
        (True, Just post)   -> do
            ((result, widget),enctype)<- runFormPost $ postMForm equation "Update post" (Just $ postContent post)
            case result of
                (FormSuccess mpost)   -> do
                    let newPost = mpost $ postCreator post
                    case (postCaptcha newPost) == captcha of
                        True -> do
                            (_) <- runDB $ replace tid (replacePostByIndex thread newPost n)
                            redirectUltDest HomeR
                        False -> do
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
        (_, _)                -> redirectUltDest HomeR


replacePostByIndex :: Thread -> Post -> Int -> Thread
replacePostByIndex t@(Thread _ _ Nothing _ _ _ _) _ _       = t
replacePostByIndex t@(Thread _ _ (Just ps) _ _ _ _) new_p i =
    case splitAt i ps of
      (_, [])         -> t
      ([], _) | i < 0 -> t
      (xs,(_:ys)) -> t { threadPosts = Just $ xs ++ (new_p:ys) }
