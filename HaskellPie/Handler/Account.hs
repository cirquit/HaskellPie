module Handler.Account where

import Authentification (getValidPersonBy404Session)
import CustomForms (updatePasswordField)
import Import
import Widgets (accountLinksW, postWidget, threadListWidget)
import Helper (dupe)

getAccountR :: Handler Html
getAccountR = do

    -- db & auth
    (person, personThreads) <- runDB $ do
        (Just (_,person)) <- getValidPersonBy404Session
        pThreads <- selectList [ThreadCreator ==. (Just $ personNick person)] [Desc ThreadLastUpdate]
        return (person, pThreads)

    -- form
    (widget, enctype) <- generateFormPost $ updateAccountInfoMForm person

    -- widgets
    let headline = "You are logged in!" :: Text
        leftWidget = postWidget enctype widget
        rightWidget = threadListWidget personThreads 15
    defaultLayout $(widgetFile "left-right-layout")

postAccountR :: Handler Html
postAccountR = do

    -- db & auth
    (pid, person, personThreads) <- runDB $ do
        (Just (pid, person)) <- getValidPersonBy404Session
        pThreads <- selectList [ThreadCreator ==. (Just $ personNick person)] [Desc ThreadLastUpdate]
        return (pid, person, pThreads)

    -- form
    ((res, widget), enctype) <- runFormPost $ updateAccountInfoMForm person
    case res of
        (FormSuccess (Person _ pw email info permissions)) -> do
            (_) <- runDB $ update pid $ [PersonPassword =. pw, PersonEmail =. email, PersonInfo =. info, PersonPermission =. permissions]
            let headline = "Your information was updated!" :: Text
                leftWidget = postWidget enctype widget
                rightWidget = [whamlet| <span> These are your threads|] >> threadListWidget personThreads 15
            defaultLayout $(widgetFile "left-right-layout")
        (FormFailure (err:_))   -> do
            let headline = err
                leftWidget = postWidget enctype widget
                rightWidget = threadListWidget personThreads 15
            defaultLayout $(widgetFile "left-right-layout")
        (_)                     -> do
            let headline = "Something went wrong, please try again." :: Text
                leftWidget = postWidget enctype widget
                rightWidget = threadListWidget personThreads 15
            defaultLayout $(widgetFile "left-right-layout")


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