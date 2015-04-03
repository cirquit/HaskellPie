module Handler.Account where

import Import
import Widgets (accountLinksW)
import CustomForms (updatePasswordField, lengthTextField)
import Helper (dupe)

getAccountR :: Handler Html
getAccountR = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick) -> do
            mperson <- runDB $ getBy $ UniqueNick nick
            case mperson of
                (Just (Entity _ person)) -> do
                    (widget, enctype) <- generateFormPost $ updateAccountInfoMForm person
                    let content = [whamlet|
                        <div style="margin:30px 0px 0px 15px;">
                            <span class=simpleBlack> You are logged in #{nick}!
                            <form method=post enctype=#{enctype}>
                                ^{widget}
                                  |]
                    defaultLayout $(widgetFile "account")
                (_)                      -> do
                    deleteSession "_ID"
                    redirect LogInR
        (_)         -> redirect LogInR

postAccountR :: Handler Html
postAccountR = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick)-> do
            mperson <- runDB $ getBy $ UniqueNick nick
            case mperson of
                (Just (Entity pid person)) -> do
                    ((res, widget), enctype) <- runFormPost $ updateAccountInfoMForm person
                    case res of
                        (FormSuccess newPerson) -> do
                            (_) <- runDB $ replace pid $ newPerson
                            let content = [whamlet|
                                <div style="margin:30px 0px 0px 15px;">
                                    <span class=simpleBlack> Your information was updated
                                          |]
                            defaultLayout $(widgetFile "account")
                        (FormFailure (err:_))   -> do
                            let content = [whamlet|
                                <div style="margin:30px 0px 0px 15px;">
                                    <span class=simpleBlack> #{err}
                                    <form method=post enctype=#{enctype}>
                                        ^{widget}
                                          |]
                            defaultLayout $(widgetFile "login")
                        (_)                     -> do
                            let content = [whamlet|
                                  <div style="margin:30px 0px 0px 15px;">
                                      <span class=simpleBlack> Something went wrong, please try again
                                      <form method=post enctype=#{enctype}>
                                          ^{widget}
                                            |]
                            defaultLayout $(widgetFile "login")
                (_) -> do
                    deleteSession "_ID"
                    redirect LogInR
        (_)                                     -> do
            redirect LogInR


updateAccountInfoMForm :: Person -> Form Person
updateAccountInfoMForm (Person nick password email (Info subject degree semCount)) token = do
    (passwordResult, passwordView) <- mreq (updatePasswordField password) "" Nothing
    (emailResult, emailView)       <- mopt emailField "" (Just email)
    (subjectResult, subjectView)   <- mopt (lengthTextField  MsgSubjectError) "" (Just subject)
    (degreeResult, degreeView)     <- mopt (selectFieldList [dupe ("Student"::Text), dupe ("Bachelor"::Text) , dupe ("Master"::Text), dupe ("Other"::Text)]) "" (Just degree)
    (semCountResult, semCountView) <- mopt (selectFieldList [(pack (show x)::Text,x) | x <- [1..20]]) "" (Just semCount)
    let info = Info <$> subjectResult <*> degreeResult <*> semCountResult
        person = Person <$> (FormSuccess nick) <*> passwordResult <*> emailResult <*> info
        widget = [whamlet|
    #{token}
    <div style="margin:15px 0px 0px 15px;">
        <table>
            <tr>
                ^{fvInput passwordView}
            <tr>
                <td> This will be your email:
                <td> ^{fvInput emailView}
            <tr>
                <td> This will be your subject:
                <td> ^{fvInput subjectView}
            <tr>
                <td> This will be your degree:
                <td> ^{fvInput degreeView}
            <tr>
                <td> This will be your semester count:
                <td> ^{fvInput semCountView}
        <input type=submit value="Update account">
                 |]
    return (person, widget)