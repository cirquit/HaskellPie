module Handler.EditPost where

import Import
import CustomForms (postMForm)
import Widgets (threadWidget, postWidget, accountLinksW)
import Helper (getPostByIndex, getPostPermissions, isModeratorBySession)

getEditPostR :: ThreadId -> Int -> Handler Html
getEditPostR tid n = do
    (thread, isMod) <- runDB $ do
        t      <- get404 tid
        isMod  <- isModeratorBySession
        return (t, isMod)
    let mpost = getPostByIndex thread n
    auth <- getPostPermissions thread n
    case (auth, mpost) of
        (True, Just post) -> do
            (widget, enctype) <- generateFormPost $ postMForm "Update post" (Just $ postContent post)
            let headline = threadTitle thread
                leftWidget = threadWidget isMod tid thread
                rightWidget = postWidget enctype widget
            defaultLayout $(widgetFile "left-right-layout")
        (_,_)                    -> redirectUltDest HomeR


postEditPostR :: ThreadId -> Int -> Handler Html
postEditPostR tid n = do
    (thread, isMod) <- runDB $ do
        t <- get404 tid
        isMod <- isModeratorBySession
        return (t, isMod)
    let oldPost = getPostByIndex thread n
    auth <- getPostPermissions thread n
    case (auth, oldPost) of
        (True, Just post)   -> do
            ((result, widget),enctype)<- runFormPost $ postMForm "Update post" (Just $ postContent post)
            case result of
                (FormSuccess mpost)   -> do
                    let newPost = mpost $ postCreator post
                    (_) <- runDB $ replace tid (replacePostByIndex thread newPost n)
                    redirectUltDest HomeR
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
replacePostByIndex t@(Thread _ _ Nothing _ _ _) _ _       = t
replacePostByIndex t@(Thread _ _ (Just ps) _ _ _) new_p i =
    case splitAt i ps of
      (_, [])         -> t
      ([], _) | i < 0 -> t
      (xs,(_:ys)) -> t { threadPosts = Just $ xs ++ (new_p:ys) }
