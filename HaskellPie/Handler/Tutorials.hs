module Handler.Tutorials where

import Import
import Widgets (accountLinksW)

getTutorialsR :: Handler Html
getTutorialsR = do
    let content = [whamlet|<span>|]
    defaultLayout $(widgetFile "homepage")
