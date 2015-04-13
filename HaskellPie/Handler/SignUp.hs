module Handler.SignUp where

import Import
import Helper (dupe)
import CustomForms (initPasswordField)
import Widgets (accountLinksW, postWidget)
--import Crypto.PasswordStore (makePassword)

getSignUpR :: Handler Html
getSignUpR = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just _) -> do
            redirect AccountR
        (_)         -> do
            (widget, enctype) <- generateFormPost signupMForm
            let headline = "You can create an account here" :: Text
                leftWidget = postWidget enctype widget
                rightWidget = [whamlet| <span> |]
            defaultLayout $(widgetFile "left-right-layout")

postSignUpR :: Handler Html
postSignUpR = do
    ((res, widget), enctype) <- runFormPost signupMForm
    case res of
        (FormSuccess (Person nick pw email (Info subject degree semCountResult) perms)) -> do
            mnick <- runDB $ getBy $ UniqueNick nick
            case mnick of
                (Just _) -> do
                    let headline = "You can create an account here" :: Text
                        leftWidget = [whamlet| <span .simpleBlack> This username is already taken, sorry.|] >> postWidget enctype widget
                        rightWidget = [whamlet| <span> |]
                    defaultLayout $(widgetFile "left-right-layout")
                (_)      -> do
                    (_) <- runDB $ insert (Person nick pw email (Info subject degree semCountResult) perms)
                    setSession "_ID" nick
                    redirectUltDest HomeR
        (FormFailure (err:_)) -> do
            let headline = "You can create an account here" :: Text
                leftWidget = [whamlet| <span .simpleBlack> #{err}|] >> postWidget enctype widget
                rightWidget = [whamlet| <span> |]
            defaultLayout $(widgetFile "left-right-layout")
        (_) -> do
            let headline = "You can create an account here" :: Text
                leftWidget = [whamlet| <span .simpleBlack> Some error happend, please try again |] >> postWidget enctype widget
                rightWidget = [whamlet| <span> |]
            defaultLayout $(widgetFile "left-right-layout")



signupMForm :: Form Person
signupMForm token = do
    (nickResult, nickView)         <- mreq textField "" Nothing
    (passwordResult, passwordView) <- mreq initPasswordField  "" Nothing
    (emailResult, emailView)       <- mopt emailField "" Nothing
    (subjectResult, subjectView)   <- mopt textField "" Nothing
    (degreeResult, degreeView)     <- mopt (selectFieldList [dupe ("Student"::Text), dupe ("Bachelor"::Text) , dupe ("Master"::Text), dupe ("Other"::Text)]) "" Nothing
    (semCountResult, semCountView) <- mopt (selectFieldList [(pack (show x)::Text,x) | x <- [1..20]]) "" Nothing
    let degree = Info <$> subjectResult <*> degreeResult <*> semCountResult
        person = Person <$> nickResult <*> passwordResult <*> emailResult <*> degree <*> pure 3
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
