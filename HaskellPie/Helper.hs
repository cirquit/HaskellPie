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
passwordsFormMatch (Person _ curPw _ _) (FormSuccess newPw) = newPw == curPw
passwordsFormMatch _  _                                     = False

spacesToMinus :: Text -> Text
spacesToMinus ys = pack $ foldr (\x xs -> if x == ' ' then '-':xs else x:xs) [] (unpack ys)

minusToSpaces :: Text -> Text
minusToSpaces ys = pack $ foldr (\x xs -> if x == '-' then ' ':xs else x:xs) [] (unpack ys)

mayUser :: Maybe Person -> Text
mayUser (Just (Person nick _ _ _)) = nick
mayUser _                         = "Anonymus"


addPost :: Maybe [Post] -> Post -> Maybe [Post]
addPost (Just xs) x = Just (xs ++ [x])
addPost _         x = Just [x]


dateFormat = "%e %b %Y"
dateFormat :: String

dateTimeFormat = "%e %b %Y %H:%M:%S"
dateTimeFormat :: String

formatDateStr :: String -> String
formatDateStr dateString = formatTime defaultTimeLocale dateTimeFormat t
    where
        t :: UTCTime
        t = read dateString


getLatestUpdate :: Thread -> UTCTime
getLatestUpdate (Thread _ _ (Just posts) time _ _) = lasts posts postTime time
getLatestUpdate (Thread _ _ _ time _ _)         = time

lasts :: [a] -> (a -> b) -> b -> b
lasts [] _ ifEmpty = ifEmpty
lasts l f _ = f $ last l

nickPersonMatch :: (Maybe Text, Maybe Person) -> Bool
nickPersonMatch ((Just nick),(Just (Person pnick _ _ _))) = nick == pnick
nickPersonMatch _                                         = False

isPostAuthor :: (Maybe Text, Maybe Post) -> Bool
isPostAuthor ((Just nick),(Just (Post _ _  (Just (Person pnick _ _ _))))) = nick == pnick
isPostAuthor _                                                            = False

getPostByIndex :: Thread -> Int -> Maybe Post
getPostByIndex _              (\x -> x<0 -> True)  = Nothing
getPostByIndex (Thread _ _ Nothing _ _ _)             _ = Nothing
getPostByIndex (Thread _ _ (Just []) _ _ _)             _ = Nothing
getPostByIndex (Thread _ _ (Just (p:_)) _ _ _)         0 = Just p
getPostByIndex t@(Thread _ _ (Just (_:ps)) _ _ _)       n = getPostByIndex (t {threadPosts = (Just ps)}) (n-1)

isPostAuthor :: MonadHandler m => Thread -> Int -> m Bool
isPostAuthor thread n = do
    mnick <- lookupSession "_ID"
    let mpost = getPostByIndex thread n
    case (mnick, mpost) of
        (Just nick, Just (Post _ _ (Just (Person pnick _ _ _)))) -> return $ nick == pnick
        (_, _)                              -> return False

deleteByIndex :: [a] -> Int -> [a]
deleteByIndex xs i = take i xs ++ drop (i+1) xs
