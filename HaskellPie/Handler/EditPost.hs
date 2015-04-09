module Handler.EditPost where

import Import
import CustomForms (postMForm)
import Widgets (threadWidget, postWidget, accountLinksW)
import Helper (getPostByIndex, isPostAuthor)

getEditPostR :: ThreadId -> Int -> Handler Html
getEditPostR tid n = do
    thread <- runDB $ get404 tid
    let mpost = getPostByIndex thread n
    isAuthor <- isPostAuthor thread n
    case (isAuthor, mpost) of
        (True, Just post) -> do
            (widget, enctype) <- generateFormPost $ postMForm "Update post" (Just $ postContent post)
            let headline = threadTitle thread
                leftWidget = threadWidget tid thread
                rightWidget = postWidget enctype widget
            defaultLayout $(widgetFile "forum")
        (_,_)                    -> redirectUltDest HomeR


postEditPostR :: ThreadId -> Int -> Handler Html
postEditPostR tid n = do
    thread <- runDB $ get404 tid
    let oldPost = getPostByIndex thread n
    isAuthor <- isPostAuthor thread n
    case (isAuthor, oldPost) of
        (True, Just post)   -> do
            ((result, widget),enctype)<- runFormPost $ postMForm "Update post" (Just $ postContent post)
            case result of
                (FormSuccess mpost)   -> do
                    let newPost = mpost $ postCreator post
                    runDB $ replace tid (replacePostByIndex thread newPost n)
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
        (_, _)                -> redirectUltDest HomeR


replacePostByIndex :: Thread -> Post -> Int -> Thread
replacePostByIndex t@(Thread _ _ Nothing _ _ _) _ _       = t
replacePostByIndex t@(Thread _ _ (Just ps) _ _ _) new_p i =
    case splitAt i ps of
      (_, [])         -> t
      ([], _) | i < 0 -> t
      (xs,(_:ys)) -> t { threadPosts = Just $ xs ++ (new_p:ys) }
