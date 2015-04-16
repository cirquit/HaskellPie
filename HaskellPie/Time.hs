module Time (formatDateStr, getLatestUpdate, calculateDiffTime) where

import Import hiding (last)
import Prelude (read)
import Data.Time
import Data.List (last)

calculateDiffTime :: UTCTime -> Thread -> String
calculateDiffTime cur thread
        | diff < 60 = show diff ++ " second(s)"
        | diff < 3600 = show (diff `div` 60) ++ " minute(s)"
        | diff < 86400 = show (diff `div` 3600) ++ " hour(s)"
        | diff < 604800 = show (diff `div` 86400) ++ " day(s)"
        | diff < 2419200 = show (diff `div` 604800) ++ " week(s)"
        | diff < 29030400 = show (diff `div` 2419200) ++ " month(s)"
        | otherwise = show (diff `div` 29030400) ++ " year(s)"
    where diff :: Integer
          diff = round $ diffUTCTime cur (getLatestUpdate thread)

getLatestUpdate :: Thread -> UTCTime
getLatestUpdate (Thread _ _ (Just posts) time _ _ _ ) = lasts posts postTime time
getLatestUpdate (Thread _ _ _ time _ _ _ )            = time

dateFormat :: String
dateFormat = "%e %b %Y"

dateTimeFormat :: String
dateTimeFormat = "%e %b %y %H:%M:%S"

formatDateStr :: String -> String
formatDateStr dateString = formatTime defaultTimeLocale dateTimeFormat t
    where
        t :: UTCTime
        t = read dateString

lasts :: [a] -> (a -> b) -> b -> b
lasts [] _ ifEmpty = ifEmpty
lasts l  f _       = f $ last l