module Handler.SignUp where

import Import
import Helper (dupe)
import CustomForms (lengthTextField, confirmPasswordField)
import Widgets (accountLinksW)
--import Crypto.PasswordStore (makePassword)

getSignUpR :: Handler Html
getSignUpR = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just _) -> do
            redirect AccountR
        (_)         -> do
            (widget, enctype) <- generateFormPost signupMForm
            let content = [whamlet|
                <div style="margin:30px 0px 0px 15px;">
                    <span class=simpleBlack> You can create an account here!
                    <form method=post enctype=#{enctype}>
                        ^{widget}
                          |]
            defaultLayout $(widgetFile "signup")

postSignUpR :: Handler Html
postSignUpR = do
    ((res, widget), enctype) <- runFormPost signupMForm
    case res of
        (FormSuccess (Person nick pw email (Info subject degree semCountResult))) -> do
            mnick <- runDB $ getBy $ UniqueNick nick
            case mnick of
                (Just _) -> do
                    let content = [whamlet|
                        <div style="margin:30px 0px 0px 15px;">
                            <span class=simpleBlack> This username is already taken, sorry.
                            <form method=post enctype=#{enctype}>
                                ^{widget}
                                  |]
                    defaultLayout $(widgetFile "signup")
                (_)      -> do
                    (_) <- runDB $ insert (Person nick pw email (Info subject degree semCountResult))
                    setSession "_ID" nick
                    let content = [whamlet|
                        <div style="margin:30px 0px 0px 15px;">
                            <span> #{show nick}
                            <span> #{show pw}
                            <span> #{show email}
                            <span> #{show subject}
                            <span> #{show degree}
                            <span> #{show semCountResult}
                                  |]
                    defaultLayout $(widgetFile "signup")
        (FormFailure (err:_)) -> do
            let content = [whamlet|
                <div style="margin:30px 0px 0px 15px;">
                    <span class=simpleBlack> #{err}
                    <form method=post enctype=#{enctype}>
                        ^{widget}
                          |]
            defaultLayout $(widgetFile "signup")
        (_) -> do
            let content = [whamlet|
                <div style="margin:30px 0px 0px 15px;">
                    <span class=simpleBlack> Some error happend, please try again!
                    <form method=post enctype=#{enctype}>
                        ^{widget}
                          |]
            defaultLayout $(widgetFile "signup")



signupMForm :: Form Person
signupMForm token = do
    (nickResult, nickView)         <- mreq (lengthTextField MsgNickError) "" Nothing
    (passwordResult, passwordView) <- mreq confirmPasswordField  "" Nothing
    (emailResult, emailView)       <- mopt emailField "" Nothing
    (subjectResult, subjectView)   <- mopt (lengthTextField  MsgSubjectError) "" Nothing
    (degreeResult, degreeView)     <- mopt (selectFieldList [dupe ("Student"::Text), dupe ("Bachelor"::Text) , dupe ("Master"::Text), dupe ("Other"::Text)]) "" Nothing
    (semCountResult, semCountView) <- mopt (selectFieldList [(pack (show x)::Text,x) | x <- [1..20]]) "" Nothing
    let degree = Info <$> subjectResult <*> degreeResult <*> semCountResult
        person = Person <$> nickResult <*> passwordResult <*> emailResult <*> degree
        widget = [whamlet|
    #{token}
    <div style="margin:15px 0px 0px 15px;">
        <table>
            <tr>
                <td> This will be your unique nickname:
                <td> ^{fvInput nickView}
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
        <input type=submit value="Create account">
                 |]
    return (person, widget)
