{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Cordova.Internal where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Prim (fromJSInt)
import Control.Monad (forM, guard, join)
import Data.Maybe (catMaybes, listToMaybe)

type JSEither e a = JSRef (Either e a)

-- | Unmarshalls [0, x] into "Right x", and [non-zero, x] into "Left x".
fromJSEither :: JSEither (JSRef e) (JSRef a) -> IO (Either (JSRef e) (JSRef a))
fromJSEither ary = do
  code <- indexArray 0 $ castRef ary
  case fromJSInt code of
    0 -> fmap Right $ indexArray 1 $ castRef ary
    _ -> fmap Left  $ indexArray 1 $ castRef ary

js_fromEnum :: (Enum a, Bounded a, ToJSRef a) => JSRef a -> IO (Maybe a)
js_fromEnum ref = fmap (listToMaybe . catMaybes) $
  forM [minBound .. maxBound] $ \x -> do
    xref <- toJSRef x
    return $ guard (eqRef ref xref) >> Just x

fromProp :: (FromJSRef b) => String -> JSRef a -> IO (Maybe b)
fromProp k obj = getProp k obj >>= fromJSRef

instance FromJSRef a => FromJSRef (Maybe a) where
  fromJSRef r = if eqRef r jsNull || eqRef r jsUndefined
    then return $ Just Nothing
    else do
      mx <- fromJSRef $ castRef r
      return $ case mx of
        Just x  -> Just (Just x)
        Nothing -> Nothing

fromRefMaybe :: (FromJSRef a) => JSRef (Maybe a) -> IO (Maybe a)
fromRefMaybe = fmap join . fromJSRef
