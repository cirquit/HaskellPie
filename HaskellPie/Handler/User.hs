module Handler.User where

import Authentification (isAdminLoggedIn)
import Import
import Widgets (threadListWidget, accountLinksW, postWidget)

getUserR :: Text -> Handler Html
getUserR nick = do

    -- db & auth
    (person, personThreads) <- runDB $ do
        (Entity _ person) <- getBy404 $ UniqueNick nick
        pThreads <- selectList [ThreadCreator ==. (Just $ personNick person)] [Desc ThreadLastUpdate]
        return (person, pThreads)
    isAdmin <- isAdminLoggedIn

    -- form
    (widget, enctype) <- generateFormPost $ updatePermissionsMForm person

    -- widgets
    let infoList = accountWidget person
        leftWidget = case isAdmin of
                         True -> infoList >> postWidget enctype widget
                         (_)  -> infoList
        headline = nick ++ pack "'s Profile"
        rightWidget = threadListWidget personThreads 15
    defaultLayout $(widgetFile "left-right-layout")


postUserR :: Text -> Handler Html
postUserR nick = do

    -- db & auth
    (pid, person, personThreads) <- runDB $ do
        (Entity pid person) <- getBy404 $ UniqueNick nick
        personThreads <- selectList [ThreadCreator ==. (Just $ personNick person)] [Desc ThreadLastUpdate]
        return (pid, person, personThreads)
    isAuthor <- isAdminLoggedIn

    -- form
    case isAuthor of
        True -> do
            ((res, widget),enctype) <- runFormPost $ updatePermissionsMForm person
            case res of
                (FormSuccess (Person _ _ _ _ permissions))  -> do
                    (_) <- runDB $ update pid [PersonPermission =. permissions]
                    let headline = nick ++ pack "'s Profile"
                        leftWidget = accountWidget person >> postWidget enctype widget
                        rightWidget = [whamlet|<span> #{nick}'s threads|] >> threadListWidget personThreads 15
                    defaultLayout $(widgetFile "left-right-layout")
                (FormFailure (err:_)) -> do
                    let headline = err
                        leftWidget = accountWidget person >> postWidget enctype widget
                        rightWidget = [whamlet|<span> #{nick}'s threads|] >> threadListWidget personThreads 15
                    defaultLayout $(widgetFile "left-right-layout")
                (_)                   -> do
                    let headline = "Something went wrong, please try again" :: Text
                        leftWidget = accountWidget person >> postWidget enctype widget
                        rightWidget = [whamlet|<span> #{nick}'s threads|] >> threadListWidget personThreads 15
                    defaultLayout $(widgetFile "left-right-layout")
        (_)  -> redirect NoPermissionsR


updatePermissionsMForm :: Person -> Form Person
updatePermissionsMForm (Person nick password email info permissions) token = do
    (permissionsResult, permissionsView)  <- mreq (selectFieldList [(("User" :: Text),3), (("Moderator" :: Text), 2), (("Admin" :: Text), 1)]) "" (Just permissions)
    let result = Person nick password email info <$> permissionsResult
        widget = [whamlet|
            #{token}
                <span> Permissions: ^{fvInput permissionsView}
            <input type=submit value="Update permissions">
                 |]
    return (result, widget)



accountWidget :: Person -> Widget
accountWidget person = [whamlet|
    <ul>
        $maybe email <- personEmail person
            <li> E-Mail: #{email}
        $maybe subject <- infoSubject $ personInfo person
            <li> Subject: #{subject}
        $maybe degree <- infoDegree $ personInfo person
            <li> Degree: #{degree}
        $maybe semCount <- infoSemesterCount $ personInfo person
            <li> Semestercount: #{show semCount}
                        |]
















