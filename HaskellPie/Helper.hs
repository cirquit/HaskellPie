module Helper where

import Import

data LoginData = LoginData {nick :: Text, pw :: Text}

dupe :: a -> (a,a)
dupe x = (x,x)

passwordsEntityMatch :: Text -> Maybe (Entity Person) -> Bool
passwordsEntityMatch pw (Just (Entity _ person)) = (personPassword person) == pw
passwordsEntityMatch _  _                        = False

passwordsFormMatch ::  Person -> FormResult Text -> Bool
passwordsFormMatch (Person _ curPw _ _) (FormSuccess newPw) = newPw == curPw
passwordsFormMatch _  _                                     = False


