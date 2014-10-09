module System.Cordova.Internal where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Prim

data JSEither_ e a
type JSEither e a = JSRef (JSEither_ e a)

-- | Unmarshalls an Either stored in JS
-- where "Right x" is [0, x] and "Left x" is [non-zero int, x].
fromJSEither :: JSEither (JSRef e) (JSRef a) -> IO (Either (JSRef e) (JSRef a))
fromJSEither ary = do
  code <- indexArray 0 $ castRef ary
  case fromJSInt code of
    0 -> fmap Right $ indexArray 1 $ castRef ary
    _ -> fmap Left  $ indexArray 1 $ castRef ary
