module Handler.LogIn where

import Authentification (passwordsEntityMatch)
import Helper
import Import
import Widgets (accountLinksW, postWidget)

getLogInR :: Handler Html
getLogInR = do

    mnick <- lookupSession "_ID"

    -- form
    case mnick of
        (Just _) -> redirect AccountR
        (_)      -> do
            (widget, enctype) <- generateFormPost signupMForm

            -- widgets
            let headline = "Please log in" :: Text
                leftWidget = postWidget enctype widget
                rightWidget = [whamlet|<span>|]
            defaultLayout $(widgetFile "left-right-layout")

postLogInR :: Handler Html
postLogInR = do

    -- form
    ((res, widget), enctype) <- runFormPost signupMForm
    case res of
        (FormSuccess (LoginData nick pw))   -> do
            mperson <- runDB $ getBy $ UniqueNick nick
            case mperson of
                (passwordsEntityMatch pw -> True) -> do
                    setSession "_ID" nick
                    redirectUltDest HomeR
                (_)                         -> do
                    let headline = "Please log in" :: Text
                        leftWidget = [whamlet|<span .simpleBlack> Something doesn't match up, please try again. |] >> postWidget enctype widget
                        rightWidget = [whamlet|<span>|]
                    defaultLayout $(widgetFile "left-right-layout")
        (_)                                 -> do
            let headline = "Please log in" :: Text
                leftWidget = [whamlet| <span .simpleBlack> Something doesn't match up, please try again.|] >> postWidget enctype widget
                rightWidget = [whamlet|<span>|]
            defaultLayout $(widgetFile "left-right-layout")

signupMForm :: Form LoginData
signupMForm token = do
    (nickResult, nickView)         <- mreq textField "" Nothing
    (passwordResult, passwordView) <- mreq passwordField "" Nothing
    let result = LoginData <$> nickResult <*> passwordResult
        widget = [whamlet|
            #{token}
                <table>
                    <tr>
                        <td> Your nick:
                        <td> ^{fvInput nickView}
                    <tr>
                        <td> Your password:
                        <td> ^{fvInput passwordView}
                <input type=submit value="Log in!">
                 |]
    return (result, widget)