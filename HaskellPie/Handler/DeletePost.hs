module Handler.DeletePost where

import Import

getDeletePostR :: ThreadId -> Int ->  Handler Html
getDeletePostR tid n = redirectToPost $ DeletePostR tid n

postDeletePostR :: ThreadId -> Int ->  Handler Html
postDeletePostR tid n = do
    mnick <- lookupSession "_ID"
    thread <- runDB $ get404 tid
    case (mnick, threadCreator thread) of
        (Just nick, Just person) -> case nick == personNick person of
                                        True -> do
                                            runDB $ replace tid (removePost thread n)
                                            redirectUltDest HomeR
                                        _    -> redirectUltDest HomeR
        (_, _)                   -> redirectUltDest HomeR




removePost :: Thread -> Int -> Thread
removePost thread@(Thread _ _ (Just l) _ _ _) n = thread {threadPosts = Just (deleteByIndex l n)}
removePost thread@(Thread _ _ Nothing _ _ _) _ = thread



deleteByIndex :: [a] -> Int -> [a]
deleteByIndex []                   _ = []
deleteByIndex l  (\x -> 0>x -> True) = l
deleteByIndex (_:ls)               0 = ls
deleteByIndex (l:ls)               n = deleteByIndex ls (n-1) ++ [l]