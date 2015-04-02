module Handler.Blog where

import Import
import Widgets (accountLinksW)

getBlogR :: Handler Html
getBlogR = defaultLayout $(widgetFile "blog")