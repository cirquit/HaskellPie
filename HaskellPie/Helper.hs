module Helper where

import Import
import Prelude (read)

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