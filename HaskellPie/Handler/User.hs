module Handler.User where

import Import
import Widgets (threadListWidget, accountLinksW)
getUserR :: Text -> Handler Html
getUserR nick = do
    (Entity _ person) <- runDB $ getBy404 $ UniqueNick nick
    personThreads <- runDB $ selectList [ThreadCreator ==. (Just person)] [Desc ThreadLastUpdate]
    let headline = nick ++ pack "'s Profile"
        leftWidget  = [whamlet|
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
        rightWidget = [whamlet|<span> #{nick}'s threads|] >> threadListWidget personThreads
    defaultLayout $(widgetFile "forum")
