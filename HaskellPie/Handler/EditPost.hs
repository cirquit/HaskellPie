module Handler.EditPost where

import Import
import CustomForms (postMForm)
import Widgets (threadWidget, postWidget, accountLinksW)
import Helper (getPostByIndex)

getEditPostR :: ThreadId -> Int -> Handler Html
getEditPostR tid n = do
    mnick <- lookupSession "_ID"
    thread <- runDB $ get404 tid
    let mpost = getPostByIndex thread n
    case (mnick, mpost) of
        (Just nick, Just post) -> do
            case postCreator post of
                (Just person) ->
                    case nick == personNick person of
                        True -> do
                            (widget, enctype) <- generateFormPost $ postMForm "Update post" (Just $ postContent post)
                            let headline = threadTitle thread
                                leftWidget = threadWidget tid thread
                                rightWidget = postWidget enctype widget
                            defaultLayout $(widgetFile "forum")
                        (_) -> redirectUltDest HomeR
                (_) -> redirectUltDest HomeR
        (_,_)                    -> redirectUltDest HomeR


postEditPostR :: ThreadId -> Int -> Handler Html
postEditPostR tid n = do
    mnick <- lookupSession "_ID"
    thread <- runDB $ get404 tid
    let ipost = getPostByIndex thread n
    case (mnick, ipost) of
        (Just nick, Just post)   -> do
            case postCreator post of
                (Just person) -> do
                    case nick == personNick person of
                        True -> do
                            ((result, widget),enctype)<- runFormPost $ postMForm "Update post" (Just $ postContent post)
                            case result of
                                (FormSuccess mpost)   -> do
                                    let newpost = mpost $ postCreator post
                                    runDB $ replace tid (replacePostByIndex thread newpost n)
                                    redirectUltDest HomeR
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
                (_)  -> redirectUltDest HomeR
        (_, _)                -> redirectUltDest HomeR




replacePostByIndex :: Thread -> Post -> Int -> Thread
replacePostByIndex t@(Thread _ _ Nothing _ _ _) _ _       = t
replacePostByIndex t@(Thread _ _ (Just ps) _ _ _) new_p i =
    case splitAt i ps of
      (_, [])         -> t
      ([], _) | i < 0 -> t
      (xs,(_:ys)) -> t { threadPosts = Just $ xs ++ (new_p:ys) }
