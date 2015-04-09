module Handler.DeleteThread where

import Import

getDeleteThreadR :: ThreadId -> Handler Html
getDeleteThreadR tid = redirectToPost $ DeleteThreadR tid

postDeleteThreadR :: ThreadId -> Handler Html
postDeleteThreadR tid = do
    mnick <- lookupSession "_ID"
    thread <- runDB $ get404 tid
    case (mnick, threadCreator thread) of
        (Just nick, Just person) -> case nick == personNick person of
                                        True -> do
                                            runDB $ delete tid
                                            redirect ForumR
                                        _    -> redirectUltDest HomeR
        (_, _)                   -> redirectUltDest HomeR


