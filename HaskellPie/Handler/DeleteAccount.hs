module Handler.DeleteAccount where

import Authentification (passwordsFormMatch, getValidPersonBySession)
import Import
import Widgets (accountLinksW, postWidget)

getDeleteAccountR :: Handler Html
getDeleteAccountR = do

    -- db
    mperson <- runDB $ getValidPersonBySession

    -- form
    case mperson of
        (Just (_,person)) -> do
            (widget, enctype) <- generateFormPost $ confirmPasswordMForm $ personPassword person
            let content = postWidget enctype widget
            defaultLayout $(widgetFile "home")
        (_)               -> do
            setUltDestCurrent
            deleteSession "_ID"
            redirect LogInR


postDeleteAccountR :: Handler Html
postDeleteAccountR = do
    -- db
    mperson <- runDB $ getValidPersonBySession

    -- form
    case mperson of
        (Just (pid, person)) -> do
            ((res, widget), enctype) <- runFormPost $ confirmPasswordMForm $ personPassword person
            case res of
                (passwordsFormMatch person -> True) -> do
                    (_) <- runDB $ delete pid
                    deleteSession "_ID"
                    redirect LogInR
                (FormFailure (err:_))           -> do
                    let content = [whamlet| <span .simpleBlack> #{err} |] >> postWidget enctype widget
                    defaultLayout $(widgetFile "home")
                (_)                           -> do
                    let content = [whamlet| <span .simpleBlack> Something went wrong, please try again |] >> postWidget enctype widget
                    defaultLayout $(widgetFile "home")
        (_)       -> do
            setUltDestCurrent
            redirect LogInR

confirmPasswordMForm :: Text -> Form Text
confirmPasswordMForm pw token = do
    let msg = "Sorry, the password doesn't match" :: Text
    (pwResult, pwView) <- mreq (checkBool (==pw) msg passwordField) "" Nothing
    let widget = [whamlet|
        #{token}
            <td> Confirm your password please:
            <td> ^{fvInput pwView}
                 |]
    return (pwResult, widget)





















