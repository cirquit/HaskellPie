module Handler.DeleteAccount where

import Import
import Widgets (accountLinksW)
import Helper (passwordsFormMatch)

getDeleteAccountR :: Handler Html
getDeleteAccountR = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick) -> do
            mperson <- runDB $ getBy $ UniqueNick nick
            case mperson of
                (Just (Entity _ person)) -> do
                    (widget, enctype) <- generateFormPost $ confirmPasswordMForm $ personPassword person
                    let content = [whamlet|
                        <div style="margin 30px 0px 0px 15px">
                            <span class=simpleBlack> Gonna delete u bro'
                            <table>
                                <tr>
                                    <form method=post enctype=#{enctype}>
                                        ^{widget}
                                  |]
                    defaultLayout $(widgetFile "deleteaccount")
                (_)                      -> do
                    setUltDestCurrent
                    deleteSession "_ID"
                    redirect LogInR
        (_)         -> do
            setUltDestCurrent
            redirect LogInR


postDeleteAccountR :: Handler Html
postDeleteAccountR = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick) -> do
            mperson <- runDB $ getBy $ UniqueNick nick
            case mperson of
                (Just (Entity pid person)) -> do
                    ((res, widget), enctype) <- runFormPost $ confirmPasswordMForm $ personPassword person
                    case res of
                        (passwordsFormMatch person -> True) -> do
                            (_) <- runDB $ delete pid
                            deleteSession "_ID"
                            redirect LogInR
                        (FormFailure (err:_))           -> do
                            let content = [whamlet|
                                <div style="margin 30px 0px 0px 15px">
                                    <span class=simpleBlack> #{err}
                                    <table>
                                        <tr>
                                            <form method=post enctype=#{enctype}>
                                                ^{widget}
                                          |]
                            defaultLayout $(widgetFile "deleteaccount")
                        (_)                           -> do
                            let content = [whamlet|
                                <div style="margin 30px 0px 0px 15px">
                                    <span class=simpleBlack> Something went wrong, please try again
                                    <table>
                                        <tr>
                                            <form method=post enctype=#{enctype}>
                                                ^{widget}
                                          |]
                            defaultLayout $(widgetFile "deleteaccount")
                (_)                        -> do
                    deleteSession "_ID"
                    redirect LogInR
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





















