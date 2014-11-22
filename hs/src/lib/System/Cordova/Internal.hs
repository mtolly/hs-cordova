{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Cordova.Internal where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Prim (fromJSInt)
import Control.Monad (forM, guard, join)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Time.Clock
import Data.Time.Clock.POSIX

type JSEither e a = JSRef (Either e a)

fromJSRef' :: (FromJSRef a) => JSRef a -> IO a
fromJSRef' ref = fromJSRef ref >>= \mx -> case mx of
  Nothing -> error "fromJSRef': deserialization failed"
  Just x  -> return x

-- | Unmarshalls [0, x] into "Right x", and [non-zero, x] into "Left x".
fromJSEither :: JSEither (JSRef e) (JSRef a) -> IO (Either (JSRef e) (JSRef a))
fromJSEither ary = do
  code <- indexArray 0 $ castRef ary
  case fromJSInt code of
    0 -> fmap Right $ indexArray 1 $ castRef ary
    _ -> fmap Left  $ indexArray 1 $ castRef ary

fromJSEither' :: (FromJSRef e, FromJSRef a) =>
  JSEither (JSRef e) (JSRef a) -> IO (Either e a)
fromJSEither' ary = fromJSEither ary >>=
  either (fmap Left . fromJSRef') (fmap Right . fromJSRef')

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

foreign import javascript unsafe
  "$1.getTime()"
  js_dateToEpochMilli :: JSRef UTCTime -> IO Double

foreign import javascript unsafe
  "$1 instanceof Date"
  js_isDate :: JSRef a -> IO Bool

foreign import javascript unsafe
  "new Date($1)"
  js_epochMilliToDate :: Double -> IO (JSRef UTCTime)

instance FromJSRef UTCTime where
  fromJSRef r = do
    b <- js_isDate r
    if b
      then do
        milli <- js_dateToEpochMilli r
        return $ Just $ posixSecondsToUTCTime $ realToFrac $ milli / 1000
      else return Nothing

instance ToJSRef UTCTime where
  toJSRef utc = let
    milli = realToFrac (utcTimeToPOSIXSeconds utc) * 1000
    in js_epochMilliToDate milli
