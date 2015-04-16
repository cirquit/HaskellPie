module Handler.DeleteThread where

import Import
import Helper (getThreadPermissions, isModeratorBySession)

getDeleteThreadR :: ThreadId -> Handler Html
getDeleteThreadR tid = redirectToPost $ DeleteThreadR tid

postDeleteThreadR :: ThreadId -> Handler Html
postDeleteThreadR tid = do
    (thread, isMod) <- runDB $ do
        t <- get404 tid
        isM <- isModeratorBySession
        return (t, isM)
    auth <- getThreadPermissions thread
    case (auth || isMod) of
        True  -> do
            runDB $ delete tid
            redirect ForumR
        (_)     -> redirectUltDest HomeR


