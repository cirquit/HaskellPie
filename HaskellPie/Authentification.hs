module Authentification where

import Import
import Helper (getPostByIndex)

getPostPermissions :: MonadHandler m => Thread -> Int -> m (Bool, Maybe Post)
getPostPermissions thread n = do
    mnick <- lookupSession "_ID"
    let mpost = getPostByIndex thread n
    case (mnick, mpost) of
        (Just nick, Just p@(Post _ _ _ (Just pnick))) -> return (nick == pnick, Just p)
        (_, Just p)                                   -> return (False, Just p)
        (_, _)                                        -> return (False, Nothing)

getThreadPermissions :: MonadHandler m => Thread -> m Bool
getThreadPermissions thread = do
    mnick <- lookupSession "_ID"
    case (mnick, thread) of
        (Just nick, (Thread _ _ _ _ _ _ (Just pnick))) -> return $ (nick == pnick)
        (_, _)                                         -> return False

getValidNickBySession:: MonadHandler m => ReaderT SqlBackend m (Maybe Text)
getValidNickBySession = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick) -> do
            mperson <- getBy $ UniqueNick nick
            case mperson of
               (Just (Entity _ person)) -> return $ Just $ personNick person
               (_)               -> return $ Nothing
        (_)           -> return $ Nothing

getValidPersonBySession:: MonadHandler m => ReaderT SqlBackend m (Maybe (PersonId, Person))
getValidPersonBySession = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick) -> do
            mperson <- getBy $ UniqueNick nick
            case mperson of
               (Just (Entity pid person)) -> return $ Just (pid,person)
               (_)               -> return $ Nothing
        (_)           -> return $ Nothing

getValidPersonBy404Session:: MonadHandler m => ReaderT SqlBackend m (Maybe (PersonId, Person))
getValidPersonBy404Session = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick) -> do
            (Entity pid person) <- getBy404 $ UniqueNick nick
            return $ Just (pid, person)
        (_)           -> return $ Nothing

passwordsEntityMatch :: Text -> Maybe (Entity Person) -> Bool
passwordsEntityMatch pw (Just (Entity _ person)) = (personPassword person) == pw
passwordsEntityMatch _  _                        = False

passwordsFormMatch ::  Person -> FormResult Text -> Bool
passwordsFormMatch (Person _ curPw _ _ _) (FormSuccess newPw) = newPw == curPw
passwordsFormMatch _  _                                       = False

isModeratorBySession :: MonadHandler m => ReaderT SqlBackend m Bool
isModeratorBySession = do
    mnick <- lookupSession "_ID"
    case (isAdmin mnick, mnick) of
        (True, _)         -> return True
        (_, (Just nick)) -> do
            mperson <- getBy $ UniqueNick nick
            case mperson of
                (Just (Entity _ (Person _ _ _ _ permissions))) -> return $ hasModRights permissions
                (_)                                            -> return False
        (_,_)            -> return False

hasModRights :: Int -> Bool
hasModRights i = i < 3

isAdmin :: Maybe Text -> Bool
isAdmin (Just "rewrite") = True
isAdmin (Just "Allora")  = True
isAdmin _                = False

isAdminLoggedIn :: (MonadHandler m) => m Bool
isAdminLoggedIn = do
    mnick <- lookupSession "_ID"
    return $ isAdmin mnick

nickPersonMatch :: (Maybe Text, Maybe Person) -> Bool
nickPersonMatch ((Just nick),(Just (Person pnick _ _ _ _))) = nick == pnick
nickPersonMatch _                                           = False