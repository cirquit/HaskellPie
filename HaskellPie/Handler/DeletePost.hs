module Handler.DeletePost where

import Authentification (getPostPermissions, isModeratorBySession)
import Helper (deleteByIndex)
import Import

getDeletePostR :: ThreadId -> Int ->  Handler Html
getDeletePostR tid n = redirectToPost $ DeletePostR tid n

postDeletePostR :: ThreadId -> Int ->  Handler Html
postDeletePostR tid n = do

    -- db & auth
    (thread, isMod) <- runDB $ do
        t     <- get404 tid
        isMod <- isModeratorBySession
        return (t, isMod)
    (isAuthor,_) <- getPostPermissions thread n

    case (isAuthor || isMod) of
        True -> do
            runDB $ replace tid (removePost thread n)
            redirectUltDest HomeR
        (_)  -> redirectUltDest HomeR

removePost :: Thread -> Int -> Thread
removePost thread@(Thread _ _ (Just l) _ _ _  _) n = thread {threadPosts = Just (deleteByIndex l n)}
removePost thread@(Thread _ _ Nothing _ _ _ _) _ = thread