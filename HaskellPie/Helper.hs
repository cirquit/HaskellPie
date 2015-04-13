module Helper where

import Import hiding (last)
import Prelude (read)
import Data.List (last)

data LoginData = LoginData {nick :: Text, pw :: Text}

dupe :: a -> (a,a)
dupe x = (x,x)

passwordsEntityMatch :: Text -> Maybe (Entity Person) -> Bool
passwordsEntityMatch pw (Just (Entity _ person)) = (personPassword person) == pw
passwordsEntityMatch _  _                        = False

passwordsFormMatch ::  Person -> FormResult Text -> Bool
passwordsFormMatch (Person _ curPw _ _ _) (FormSuccess newPw) = newPw == curPw
passwordsFormMatch _  _                                       = False

spacesToMinus :: Text -> Text
spacesToMinus ys = Data.Text.map (\x xs -> if x == ' ' then '-' else x) ys

minusToSpaces :: Text -> Text
minusToSpaces ys = Data.Text.map (\x xs -> if x == '-' then ' ' else x) ys

addPost :: Maybe [Post] -> Post -> Maybe [Post]
addPost (Just xs) x = Just (xs ++ [x])
addPost _         x = Just [x]


dateFormat :: String
dateFormat = "%e %b %Y"

dateTimeFormat :: String
dateTimeFormat = "%e %b %y %H:%M:%S"

formatDateStr :: String -> String
formatDateStr dateString = formatTime defaultTimeLocale dateTimeFormat t
    where
        t :: UTCTime
        t = read dateString


getLatestUpdate :: Thread -> UTCTime
getLatestUpdate (Thread _ _ (Just posts) time _ _) = lasts posts postTime time
getLatestUpdate (Thread _ _ _ time _ _)            = time

lasts :: [a] -> (a -> b) -> b -> b
lasts [] _ ifEmpty = ifEmpty
lasts l  f _       = f $ last l

nickPersonMatch :: (Maybe Text, Maybe Person) -> Bool
nickPersonMatch ((Just nick),(Just (Person pnick _ _ _ _))) = nick == pnick
nickPersonMatch _                                           = False

-- getPostByIndex :: Thread -> Int -> Maybe Post
-- getPostByIndex (Thread _ _ Nothing _ _ _)   _ = Nothing
-- getPostByIndex (Thread _ _ (Just ps) _ _ _) i =
--     case splitAt i ps of
--       (_, [])         -> Nothing
--       ([], _) | i < 0 -> Nothing
--       (_,(y:_))       -> Just y


getPostByIndex :: Thread -> Int -> Maybe Post
getPostByIndex (Thread _ _ Nothing   _ _ _) _ = Nothing
getPostByIndex (Thread _ _ (Just ps) _ _ _) i = lookup i indexedPosts
    where indexedPosts = zip [1..] ps



getPostPermissions :: MonadHandler m => Thread -> Int -> m Bool
getPostPermissions thread n = do
    mnick <- lookupSession "_ID"
    let mpost = getPostByIndex thread n
    case isAdmin mnick of
        True -> return True
        (_)  -> case (mnick, mpost) of
                    (Just nick, Just (Post _ _ (Just (Person pnick _ _ _ _)))) -> return $ nick == pnick
                    (_, _)                              -> return False

getThreadPermissions :: MonadHandler m => Thread -> m Bool
getThreadPermissions thread = do
    mnick <- lookupSession "_ID"
    case isAdmin mnick of
        True -> return True
        (_)    -> case (mnick, thread) of
                  (Just nick, (Thread _ _ _ _ _ (Just (Person pnick _ _ _)))) -> return $ nick == pnick
                  (_, _)                                                      -> return False

deleteByIndex :: [a] -> Int -> [a]
deleteByIndex xs i = take i xs ++ drop (i+1) xs

getPersonBySession:: MonadHandler m => ReaderT SqlBackend m (Maybe Person)
getPersonBySession = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick) -> do
            mperson <- getBy $ UniqueNick nick
            case mperson of
               (Just (Entity _ person)) -> return $ Just person
               (_)               -> return $ Nothing
        (_)           -> return $ Nothing


cutBy20 :: Text -> Text
cutBy20 text
  | (length text) <= 20 = text
  | otherwise           = pack $ take 20 (unpack text) ++ "..."


isAdmin :: Maybe Text -> Bool
isAdmin (Just "rewrite") = True
isAdmin (Just "Allora")  = True
isAdmin _                = False

isAdminLoggedIn :: (MonadHandler m) => m Bool
isAdminLoggedIn = do
    mnick <- lookupSession "_ID"
    return $ isAdmin mnick