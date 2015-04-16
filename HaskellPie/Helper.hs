module Helper where

import Import
import qualified Data.Text as T

data LoginData = LoginData {nick :: Text, pw :: Text}

dupe :: a -> (a,a)
dupe x = (x,x)

spacesToMinus :: Text -> Text
spacesToMinus ys = T.map (\x -> if x == ' ' then '-' else x) ys

minusToSpaces :: Text -> Text
minusToSpaces ys = T.map (\x -> if x == '-' then ' ' else x) ys

updatePosts :: Maybe [Post] -> Post -> Maybe [Post]
updatePosts (Just xs) x = Just (xs ++ [x])
updatePosts _         x = Just [x]

getPostByIndex :: Thread -> Int -> Maybe Post
getPostByIndex (Thread _ _ Nothing   _ _ _ _) _ = Nothing
getPostByIndex (Thread _ _ (Just ps) _ _ _ _) i = lookup i indexedPosts
    where indexedPosts = zip [0..] ps

deleteByIndex :: [a] -> Int -> [a]
deleteByIndex xs i = take i xs ++ drop (i+1) xs