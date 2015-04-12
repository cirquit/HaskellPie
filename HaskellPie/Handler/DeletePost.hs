module Handler.DeletePost where

import Import
import Helper (getPostPermissions, deleteByIndex)

getDeletePostR :: ThreadId -> Int ->  Handler Html
getDeletePostR tid n = redirectToPost $ DeletePostR tid n

postDeletePostR :: ThreadId -> Int ->  Handler Html
postDeletePostR tid n = do
    thread <- runDB $ get404 tid
    auth <- getPostPermissions thread n
    case auth of
        True -> do
            runDB $ replace tid (removePost thread n)
            redirectUltDest HomeR
        (_)  -> redirectUltDest HomeR

removePost :: Thread -> Int -> Thread
removePost thread@(Thread _ _ (Just l) _ _ _) n = thread {threadPosts = Just (deleteByIndex l n)}
removePost thread@(Thread _ _ Nothing _ _ _) _ = thread