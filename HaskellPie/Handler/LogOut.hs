module Handler.LogOut where

import Import

getLogOutR :: Handler Html
getLogOutR = do
    deleteSession "_ID"
    redirect HomeR
