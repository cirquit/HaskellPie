module Helper where

import Import

data LoginData = LoginData {nick :: Text, pw :: Text}

dupe :: a -> (a,a)
dupe x = (x,x)

passwordsMatch :: Text -> Maybe (Entity Person) -> Bool
passwordsMatch pw (Just (Entity _ person)) = (personPassword person) == pw
passwordsMatch _  _                        = False