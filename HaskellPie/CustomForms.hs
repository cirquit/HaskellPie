module CustomForms where

import Import

-- | Custom Int field that doesn't allow number below 0
unsignedIntField :: ( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Int
unsignedIntField msg = checkBool (>=0) msg intField

-- | Custom textfield that doesn't allow "spaces-only"-input or inputs longer than 200 characters
lengthTextField ::( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Text
lengthTextField msg = checkBool (\x -> ((length x) <= 200) && noSpaceList x) msg textField
    where noSpaceList l = not $ all (==' ') l


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
                  <td> <input id=#{idAttr} name=#{nameAttr} type=password required value="">
              <tr>
                  <td> Please confirm:
                  <td> <input id=#{idAttr}-confirm name=#{nameAttr} type=password required value="">
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
                | a == oldpw && null b && null c      -> return $ Right $ Just a
                | a == oldpw && b == c && not (null b)-> return $ Right $ Just c
                | a /= oldpw           -> return $ Left "Your current password doesn't match"
                | otherwise            -> return $ Left "The new passwords doesn't match up"
            []  -> return $ Left "You must enter a password"
            _   -> return $ Left "Something went wrong with the password input"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
                  <td> Enter current password:
                  <td> <input id=#{idAttr} name=#{nameAttr} type=password required value="">
              <tr>
                  <td> Enter new password:
                  <td> <input id=#{idAttr} name=#{nameAttr} type=password>
              <tr>
                  <td> Please confirm:
                  <td> <input id=#{idAttr}-confirm name=#{nameAttr} type=password>
        |]
    , fieldEnctype = UrlEncoded
    }