
{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Geolocation
( Coordinates(..)
, Position(..)
, PositionError(..)
, PositionErrorCode(..)
, getCurrentPosition
, watchPosition
) where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import System.Cordova.Internal
import Data.Time.Clock
import Control.Monad ((>=>))
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal
import System.Cordova.Internal
import Data.Default
import Control.Applicative


foreign import javascript interruptible
  "navigator.geolocation.getCurrentPosition(hs_good($c), hs_error($c), $1);"
  js_getCurrentPosition
  :: JSRef GeolocationOptions
  -> IO (JSEither (JSRef PositionError) (JSRef Position))

getCurrentPosition :: GeolocationOptions -> IO (Either PositionError Position)
getCurrentPosition opts =
  toJSRef opts >>= js_getCurrentPosition >>= fromJSEither'

data Coordinates = Coordinates { latitude :: Maybe Double, longitude :: Maybe Double, altitude :: Maybe Double, accuracy :: Maybe Double, altitudeAccuracy :: Maybe Double, heading :: Maybe Double, speed :: Maybe Double } deriving (Eq, Ord, Show, Read)
instance Default Coordinates where def = Coordinates def def def def def def def
instance ToJSRef Coordinates where
  toJSRef opts = do
    obj <- newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> toJSRef x >>= \ref -> setProp s ref obj
        _set s f = toJSRef (f opts) >>= \ref -> setProp s ref obj
    _setJust "latitude" latitude
    _setJust "longitude" longitude
    _setJust "altitude" altitude
    _setJust "accuracy" accuracy
    _setJust "altitudeAccuracy" altitudeAccuracy
    _setJust "heading" heading
    _setJust "speed" speed
    return obj
instance FromJSRef Coordinates where
  fromJSRef obj = do
    _x0 <- fromProp "latitude" obj
    _x1 <- fromProp "longitude" obj
    _x2 <- fromProp "altitude" obj
    _x3 <- fromProp "accuracy" obj
    _x4 <- fromProp "altitudeAccuracy" obj
    _x5 <- fromProp "heading" obj
    _x6 <- fromProp "speed" obj
    return $ Coordinates <$> _x0 <*> _x1 <*> _x2 <*> _x3 <*> _x4 <*> _x5 <*> _x6

data Position = Position { coords :: Coordinates, timestamp :: UTCTime } deriving (Eq, Ord, Show, Read)
instance ToJSRef Position where
  toJSRef opts = do
    obj <- newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> toJSRef x >>= \ref -> setProp s ref obj
        _set s f = toJSRef (f opts) >>= \ref -> setProp s ref obj
    _set "coords" coords
    _set "timestamp" timestamp
    return obj
instance FromJSRef Position where
  fromJSRef obj = do
    _x0 <- fromProp "coords" obj
    _x1 <- fromProp "timestamp" obj
    return $ Position <$> _x0 <*> _x1

data PositionError = PositionError { code :: PositionErrorCode, message :: String } deriving (Eq, Ord, Show, Read)
instance ToJSRef PositionError where
  toJSRef opts = do
    obj <- newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> toJSRef x >>= \ref -> setProp s ref obj
        _set s f = toJSRef (f opts) >>= \ref -> setProp s ref obj
    _set "code" code
    _set "message" message
    return obj
instance FromJSRef PositionError where
  fromJSRef obj = do
    _x0 <- fromProp "code" obj
    _x1 <- fromProp "message" obj
    return $ PositionError <$> _x0 <*> _x1

data PositionErrorCode = PermissionDenied | PositionUnavailable | Timeout deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "PositionError.PERMISSIONDENIED" _PositionErrorCode_PermissionDenied :: JSRef PositionErrorCode
foreign import javascript unsafe "PositionError.POSITIONUNAVAILABLE" _PositionErrorCode_PositionUnavailable :: JSRef PositionErrorCode
foreign import javascript unsafe "PositionError.TIMEOUT" _PositionErrorCode_Timeout :: JSRef PositionErrorCode
instance ToJSRef PositionErrorCode where
  toJSRef PermissionDenied = return _PositionErrorCode_PermissionDenied
  toJSRef PositionUnavailable = return _PositionErrorCode_PositionUnavailable
  toJSRef Timeout = return _PositionErrorCode_Timeout
instance FromJSRef PositionErrorCode where
  fromJSRef = js_fromEnum

data GeolocationOptions = GeolocationOptions { enableHighAccuracy :: Maybe Bool, timeout :: Maybe Double, maximumAge :: Maybe Double } deriving (Eq, Ord, Show, Read)
instance Default GeolocationOptions where def = GeolocationOptions def def def
instance ToJSRef GeolocationOptions where
  toJSRef opts = do
    obj <- newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> toJSRef x >>= \ref -> setProp s ref obj
        _set s f = toJSRef (f opts) >>= \ref -> setProp s ref obj
    _setJust "enableHighAccuracy" enableHighAccuracy
    _setJust "timeout" timeout
    _setJust "maximumAge" maximumAge
    return obj
instance FromJSRef GeolocationOptions where
  fromJSRef obj = do
    _x0 <- fromProp "enableHighAccuracy" obj
    _x1 <- fromProp "timeout" obj
    _x2 <- fromProp "maximumAge" obj
    return $ GeolocationOptions <$> _x0 <*> _x1 <*> _x2

foreign import javascript unsafe
  "navigator.geolocation.watchPosition($1, $2, $3);"
  js_watchPosition
  :: JSFun (JSRef Position -> IO ())
  -> JSFun (JSRef PositionError -> IO ())
  -> JSRef GeolocationOptions
  -> IO (JSRef String)

foreign import javascript unsafe
  "navigator.geolocation.clearWatch($1);"
  js_clearWatch :: JSRef String -> IO ()

watchPosition
  :: GeolocationOptions
  -> (Either PositionError Position -> IO ())
  -> IO (IO ())
watchPosition opts f = do
  fnGood  <- asyncCallback1 AlwaysRetain $ fromJSRef' >=> f . Right
  fnError <- asyncCallback1 AlwaysRetain $ fromJSRef' >=> f . Left
  opts' <- toJSRef opts
  watchID <- js_watchPosition fnGood fnError opts'
  return $ do
    js_clearWatch watchID
    releaseAll fnGood
    releaseAll fnError
