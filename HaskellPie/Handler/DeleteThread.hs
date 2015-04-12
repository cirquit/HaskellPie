module Handler.DeleteThread where

import Import
import Helper (getThreadPermissions)

getDeleteThreadR :: ThreadId -> Handler Html
getDeleteThreadR tid = redirectToPost $ DeleteThreadR tid

postDeleteThreadR :: ThreadId -> Handler Html
postDeleteThreadR tid = do
    thread <- runDB $ get404 tid
    auth <- getThreadPermissions thread
    case auth of
        True  -> do
            runDB $ delete tid
            redirect ForumR
        (_)     -> redirectUltDest HomeR


