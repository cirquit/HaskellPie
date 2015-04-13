module Handler.Tutorials where

import Import
import Widgets (accountLinksW)

getTutorialsR :: Handler Html
getTutorialsR = do
    let headline = "Tutorials" :: Text
    let leftWidget = [whamlet|<span>|]
    let rightWidget = [whamlet|<span>|]
    defaultLayout $(widgetFile "left-right-layout")
