module Handler.Thread where

import Import
import Widgets (accountLinksW, postWidget)
import Helper (minusToSpaces, mayUser, addPost, formatDateStr)
import Yesod.Form.Nic (nicHtmlField)

getThreadR :: Text -> Handler Html
getThreadR text = do
    setUltDestCurrent
    (widget, enctype) <- generateFormPost $ postMForm
    let completeTitle = minusToSpaces text
    (Entity tid thread) <- runDB $ getBy404 $ UniqueTitle completeTitle
    let headline = threadTitle thread
    let leftWidget = threadWidget tid thread
    let rightWidget = postWidget enctype widget
    defaultLayout $(widgetFile "forum")

postThreadR :: Text -> Handler Html
postThreadR text = do
    let completeTitle = minusToSpaces text
    mnick <- lookupSession "_ID"
    (Entity tid thread) <- runDB $ getBy404 $ UniqueTitle completeTitle
    user <- case mnick of
                   (Just nick) -> do
                       mperson <- runDB $ getBy $ UniqueNick nick
                       case mperson of
                          (Just (Entity _ person)) -> return $ Just person
                          (_)               -> return $ Nothing
                   (_)           -> return $ Nothing
    let oldPosts = threadPosts thread
    ((result, widget), enctype) <- runFormPost $ postMForm
    (nwidget, nenctype)         <- generateFormPost $ postMForm
    case result of
        (FormSuccess mpost)   -> do
            (Entity ntid newThread) <- runDB $ do
                (_) <- update tid [ThreadPosts =. (addPost oldPosts (mpost user))]
                entity <-getBy404 $ UniqueTitle completeTitle
                return entity
            let headline    = threadTitle thread
            let leftWidget  = threadWidget ntid newThread
            let rightWidget = postWidget nenctype nwidget
            defaultLayout $(widgetFile "forum")
        (FormFailure (err:_)) -> do
            let headline    = threadTitle thread
            let leftWidget  = threadWidget tid thread
            let rightWidget = [whamlet|<span .simpleBlack> #{err}|] >> postWidget enctype widget
            defaultLayout $(widgetFile "forum")
        (_)                   -> do
            let headline    = threadTitle thread
            let leftWidget  = threadWidget tid thread
            let rightWidget = [whamlet|<span .simpleBlack> Something went wrong, please try again|] >> postWidget enctype widget
            defaultLayout $(widgetFile "forum")

postMForm :: Form (Maybe Person -> Post)
postMForm token = do
  time <- liftIO $ getCurrentTime
  (contentResult, contentView) <- mreq nicHtmlField "" Nothing
  let post = Post <$> contentResult <*> pure time
      widget = [whamlet|
              #{token}
                   ^{fvInput contentView}
                  <input type=submit value="Submit post">
               |] >> toWidget [lucius|
                       ##{fvId contentView} {
                           width:100%;
                           height:200px;
                       }
                                |]
  return (post, widget)


threadWidget :: ThreadId -> Thread -> Widget
threadWidget tid thread = do
   mnick <- lookupSession "_ID"
   nick <-  case mnick of
              (Just nick) -> return $ nick
              (_)         -> return $ ""
   let enum = [0..]::[Int]
   [whamlet|
        <div .thread_answer>
            <span .simpleCreator> #{mayUser $ threadCreator thread}
            <span .simpleTime>#{formatDateStr $ show $ threadTime thread}
            $maybe person <- threadCreator thread
                $if nick == (personNick person)
                    <a href=@{DeleteThreadR tid}> Delete thread
                $else
            $nothing
            <br>
            <span> #{threadContent thread}
    $maybe posts <- threadPosts thread
        $with enum_posts <- (zip enum posts)
            $forall (n, post) <- enum_posts
                <div .thread_answer>
                        <span .simpleCreator> #{mayUser $ postCreator post}
                        <span .simpleTime> #{formatDateStr $ show $ postTime post}
                        $maybe person <- postCreator post
                            $if nick == (personNick person)
                                <a href=@{DeletePostR tid n}> Delete
                            $else
                        $nothing
                        <br>
                        <span> #{postContent post}
                          |]
