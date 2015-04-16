module CustomForms where

import Import
import Yesod.Form.Nic (nicHtmlField)
import Helper

-- | Custom Int field that doesn't allow number below 0
unsignedIntField :: ( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Int
unsignedIntField msg = checkBool (>=0) msg intField

-- | Custom textfield that doesn't allow "spaces-only"-input or inputs longer than 200 characters
lengthTextField ::( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Text
lengthTextField msg = checkBool (\x -> ((length x) <= 200) && noSpaceList x) msg titleField
    where noSpaceList l = not $ all (==' ') l


titleField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
titleField = Field
    { fieldParse = parseHelper $ Right
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either id id val}" placeholder="Enter your title">
|]
    , fieldEnctype = UrlEncoded
    }

-- | Custom password field
--   Always required
--   made to fit in a table
initPasswordField :: Field Handler Text
initPasswordField = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [a, b]
                | a == b    -> return $ Right $ Just a
                | otherwise -> return $ Left "Passwords don't match"
            [] -> return $ Left "You must enter a password"
            _  -> return $ Left "You must enter two values"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
                  <td> Enter password:
                  <td> <input id=#{idAttr} name=#{nameAttr} type=password required value="" placeholder="Enter password">
              <tr>
                  <td> Please confirm:
                  <td> <input id=#{idAttr}-confirm name=#{nameAttr} type=password required value="" placeholder="Please confirm">
        |]
    , fieldEnctype = UrlEncoded
    }

-- | Custom update password field
--   Current password always required
--   If passwords match and second & third field is empty, return current password
--   If passwords match and second & third field match, return new password
--   made to fit a table
updatePasswordField :: Text -> Field Handler Text
updatePasswordField oldpw = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [a, b, c]
                | a == b && b == c                    -> return $ Left "Current password and new password can't be identical"
                | a == oldpw && null b && null c      -> return $ Right $ Just a
                | a == oldpw && b == c && not (null b)-> return $ Right $ Just c
                | a /= oldpw           -> return $ Left "Your current password doesn't match"
                | otherwise            -> return $ Left "The new passwords doesn't match up"
            []  -> return $ Left "You must enter a password"
            _   -> return $ Left "Something went wrong with the password input"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
                  <td> Enter current password:
                  <td> <input id=#{idAttr} name=#{nameAttr} type=password required value="" placeholder="Enter current password">
              <tr>
                  <td> Enter new password:
                  <td> <input id=#{idAttr} name=#{nameAttr} type=password placeholder="Enter new password">
              <tr>
                  <td> Please confirm:
                  <td> <input id=#{idAttr}-confirm name=#{nameAttr} type=password placeholder="Please confirm">
        |]
    , fieldEnctype = UrlEncoded
    }


postMForm :: MathEquation-> Text -> Maybe Html -> Form (Maybe Text -> Post)
postMForm equation buttontext mHTML token  = do
    time <- liftIO $ getCurrentTime
    (contentResult, contentView) <- mreq nicHtmlField "" mHTML
    (captchaResult, captchaView) <- mreq intField "" Nothing
    let post = Post <$> contentResult <*> pure time <*> captchaResult
        widget = [whamlet|
                #{token}
                     ^{fvInput contentView}
                      <span> What is #{show $ x equation} #{function equation} #{show $ y equation} = ^{fvInput captchaView} // should be #{eqResult equation}
                    <input type=submit value=#{buttontext}>
                    <br>
                <pre> Edit HTML -> &lt;pre&gt; Markup code goes here! &lt;/pre&gt; </pre>
                <br>
                <div style="width:100%; height:30px;">
                  <label style="float:left;"> <a href=http://www.simplehtmlguide.com/text.php style="margin:2px;"> Some how-to use HTML tags </a>
                  <label style="float:right;"> <a href=lpaste.net> For larger files </a>
                 |] >> toWidget [lucius|
                         ##{fvId contentView} {
                             width:100%;
                             height:200px;
                         }
                                  |]
    return (post, widget)

threadMForm :: MathEquation -> Text -> Maybe Text -> Maybe Html -> Form (Maybe Text -> Thread)
threadMForm equation buttontext mTitle mHTML token = do
    time <- liftIO $ getCurrentTime
    (titleResult, titleView) <- mreq (lengthTextField MsgSubjectError) "" mTitle
    (contentResult, contentView) <- mreq nicHtmlField "" mHTML
    (captchaResult, captchaView) <- mreq intField "" Nothing
    let thread = Thread <$> titleResult <*> contentResult <*> (pure Nothing) <*> (pure time) <*> (pure time) <*> captchaResult
        widget = [whamlet|
            #{token}
                  ^{fvInput titleView}
                  ^{fvInput contentView}
                  <span> What is #{show $ x equation} #{function equation} #{show $ y equation} = ^{fvInput captchaView} // should be #{eqResult equation}
                <input type=submit value="#{buttontext}">
            <br>
            <pre> Edit HTML -> &lt;pre&gt; Markup code goes here! &lt;/pre&gt; </pre>
            <label> <a href=http://www.simplehtmlguide.com/text.php style="margin:2px;"> Some how-to use HTML tags </a>
            <label> <a href=http://www.lpaste.net style="margin:2px;"> For larger files </a>
                 |] >> toWidget [lucius|
                       ##{fvId contentView} {
                           width:100%;
                           height:200px;
                       }
                       ##{fvId titleView} {
                          width:100%;
                       }
                                |]
    return (thread, widget)


