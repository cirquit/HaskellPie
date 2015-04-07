module Handler.LogIn where

import Import
import Widgets (accountLinksW)
import Helper

getLogInR :: Handler Html
getLogInR = do
  mnick <- lookupSession "_ID"
  case mnick of
      (Just _) -> redirect AccountR
      (_)      -> do
          (widget, enctype) <- generateFormPost signupMForm
          let content = [whamlet|
                  <form method=post enctype=#{enctype}>
                      ^{widget}
                        |]
          defaultLayout $(widgetFile "homepage")

postLogInR :: Handler Html
postLogInR = do
    ((res, widget), enctype) <- runFormPost signupMForm
    case res of
        (FormSuccess (LoginData nick pw))   -> do
            mperson <- runDB $ getBy $ UniqueNick nick
            case mperson of
                (passwordsEntityMatch pw -> True) -> do
                    setSession "_ID" nick
                    redirectUltDest HomeR
                (_)                         -> do
                    let content = [whamlet|
                            <span class=simpleBlack> Something doesn't match up, please try again.
                            <form method=post enctype=#{enctype}>
                                ^{widget}
                                  |]
                    defaultLayout $(widgetFile "homepage")
        (_)                                 -> do
            let content = [whamlet|
                    <span class=simpleBlack> Something doesn't match up, please try again.
                    <form method=post enctype=#{enctype}>
                        ^{widget}
                          |]
            defaultLayout $(widgetFile "homepage")

signupMForm :: Form LoginData
signupMForm token = do
    (nickResult, nickView)         <- mreq textField "" Nothing
    (passwordResult, passwordView) <- mreq passwordField "" Nothing
    let result = LoginData <$> nickResult <*> passwordResult
        widget = [whamlet|
            #{token}
                <table>
                    <tr>
                        <td> Your nick:
                        <td> ^{fvInput nickView}
                    <tr>
                        <td> Your password:
                        <td> ^{fvInput passwordView}
                <input type=submit value="Log in!">
                 |]
    return (result, widget)