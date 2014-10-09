module System.Cordova.Internal where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Prim (fromJSInt)

type JSEither e a = JSRef (Either e a)

-- | Unmarshalls [0, x] into "Right x", and [non-zero, x] into "Left x".
fromJSEither :: JSEither (JSRef e) (JSRef a) -> IO (Either (JSRef e) (JSRef a))
fromJSEither ary = do
  code <- indexArray 0 $ castRef ary
  case fromJSInt code of
    0 -> fmap Right $ indexArray 1 $ castRef ary
    _ -> fmap Left  $ indexArray 1 $ castRef ary
