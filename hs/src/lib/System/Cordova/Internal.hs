module System.Cordova.Internal where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Prim (fromJSInt)
import Data.Char (isUpper, isLower, toUpper)

type JSEither e a = JSRef (Either e a)

-- | Unmarshalls [0, x] into "Right x", and [non-zero, x] into "Left x".
fromJSEither :: JSEither (JSRef e) (JSRef a) -> IO (Either (JSRef e) (JSRef a))
fromJSEither ary = do
  code <- indexArray 0 $ castRef ary
  case fromJSInt code of
    0 -> fmap Right $ indexArray 1 $ castRef ary
    _ -> fmap Left  $ indexArray 1 $ castRef ary

data Window_
type Window = JSRef Window_

foreign import javascript unsafe
  "(function(){ return window; })()"
  window :: Window

getEnum :: [String] -> (a -> String) -> a -> IO (JSRef a)
getEnum props f x = getProps (props ++ [f x]) window

getProps :: [String] -> JSRef a -> IO (JSRef b)
getProps []       obj = return $ castRef obj
getProps (p : ps) obj = getProp p obj >>= getProps ps

cordovaEnum :: (Show a) => a -> String
cordovaEnum = map toUpper . underscore . show where
  underscore (x : y : z : xs)
    | isLower x && isUpper y && isUpper z
    = x : '_' : y : z : underscore xs
  underscore [x, y]
    | isLower x && isUpper y
    = [x, '_', y]
  underscore (x : xs) = x : underscore xs
  underscore ""       = ""
