module Captcha (getCaptchaBySession, createMathEq, MathEquation(..)) where

import Import
import Prelude (reads)
import System.Random (randomRIO)

data MathEquation = MathEq {x :: Int, y :: Int, eqResult :: Text, function :: Char}

maybeRead :: Read a => String -> Maybe a
maybeRead (reads -> [(x,"")]) = Just x
maybeRead _                   = Nothing

maybeInt :: String -> Maybe Int
maybeInt = maybeRead

createMathEq :: IO (MathEquation)
createMathEq = do
    a  <- randomRIO (0 :: Int, 100 :: Int)
    b  <- randomRIO (0 :: Int, 100 :: Int)
    f' <- randomRIO (0 :: Int, 2 :: Int)
    let (f, fs) = case f' of
                      0 -> ((+),'+')
                      1 -> ((-),'-')
                      _ -> ((*),'*')
        r =  pack $ show $ f a b
    return $ MathEq a b r fs

getCaptchaBySession :: MonadHandler m => m Int
getCaptchaBySession = do
    mText <- lookupSession "captcha"
    let sessiontext = fromMaybe ("" :: Text) mText
        mInt        = maybeInt $ unpack sessiontext
        result      = fromMaybe (100000::Int) mInt
    return result


