module Handler.DeleteThread where

import Authentification (getThreadPermissions, isModeratorBySession)
import Import

getDeleteThreadR :: ThreadId -> Handler Html
getDeleteThreadR tid = redirectToPost $ DeleteThreadR tid

postDeleteThreadR :: ThreadId -> Handler Html
postDeleteThreadR tid = do

    -- db & auth
    (thread, isMod) <- runDB $ do
        t <- get404 tid
        isMod <- isModeratorBySession
        return (t, isMod)
    isAuthor <- getThreadPermissions thread

    case (isAuthor || isMod) of
        True  -> do
            runDB $ delete tid
            redirect ForumR
        (_)     -> redirectUltDest HomeR


