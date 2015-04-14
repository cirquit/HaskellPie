module Handler.Account where

import Import
import Widgets (accountLinksW, postWidget, threadListWidget)
import CustomForms (updatePasswordField)
import Helper (dupe)

getAccountR :: Handler Html
getAccountR = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick) -> do
            mperson <- runDB $ getBy $ UniqueNick nick
            case mperson of
                (Just (Entity _ person)) -> do
                    personThreads <- runDB $ selectList [ThreadCreator ==. (Just $ personNick person)] [Desc ThreadLastUpdate]
                    (widget, enctype) <- generateFormPost $ updateAccountInfoMForm person
                    let headline = "You are logged in " ++ nick ++ "!"
                        leftWidget = postWidget enctype widget
                        rightWidget = [whamlet| <span> These are your threads|] >> threadListWidget personThreads
                    defaultLayout $(widgetFile "left-right-layout")
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
                    personThreads <- runDB $ selectList [ThreadCreator ==. (Just $ personNick person)] [Desc ThreadLastUpdate]
                    ((res, widget), enctype) <- runFormPost $ updateAccountInfoMForm person
                    case res of
                        (FormSuccess (Person _ pw email info permissions)) -> do
                            (_) <- runDB $ update pid $ [PersonPassword =. pw, PersonEmail =. email, PersonInfo =. info, PersonPermission =. permissions]
                            let headline = "Your information was updated!" :: Text
                                leftWidget = postWidget enctype widget
                                rightWidget = [whamlet| <span> These are your threads|] >> threadListWidget personThreads
                            defaultLayout $(widgetFile "left-right-layout")
                        (FormFailure (err:_))   -> do
                            let headline = err
                                leftWidget = postWidget enctype widget
                                rightWidget = threadListWidget personThreads
                            defaultLayout $(widgetFile "left-right-layout")
                        (_)                     -> do
                            let headline = "Something went wrong, please try again." :: Text
                                leftWidget = postWidget enctype widget
                                rightWidget = threadListWidget personThreads
                            defaultLayout $(widgetFile "left-right-layout")
                (_) -> deleteSession "_ID" >>  redirect LogInR
        (_)        -> redirect LogInR


updateAccountInfoMForm :: Person -> Form Person
updateAccountInfoMForm (Person nick password email (Info subject degree semCount) permissions) token = do
    (passwordResult, passwordView) <- mreq (updatePasswordField password) "" Nothing
    (emailResult, emailView)       <- mopt emailField "" (Just email)
    (subjectResult, subjectView)   <- mopt textField "" (Just subject)
    (degreeResult, degreeView)     <- mopt (selectFieldList [dupe ("Student"::Text), dupe ("Bachelor"::Text) , dupe ("Master"::Text), dupe ("Other"::Text)]) "" (Just degree)
    (semCountResult, semCountView) <- mopt (selectFieldList [(pack (show x)::Text,x) | x <- [1..20]]) "" (Just semCount)
    let info = Info <$> subjectResult <*> degreeResult <*> semCountResult
        person = Person <$> (pure nick) <*> passwordResult <*> emailResult <*> info <*> pure permissions
        widget = [whamlet|
    #{token}
        <table style="marin: 0px 0px 0px 30px;">
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