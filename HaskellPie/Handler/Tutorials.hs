module Handler.Tutorials where

import Import
import Widgets (accountLinksW)

getTutorialsR :: Handler Html
getTutorialsR = defaultLayout $(widgetFile "tutorials")
