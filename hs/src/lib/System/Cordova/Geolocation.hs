
{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Geolocation
( Coordinates(..)
, Position(..)
, PositionError(..)
, PositionErrorCode(..)
, GeolocationOptions(..)
, getCurrentPosition
, watchPosition
) where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import System.Cordova.Internal
import Data.Time.Clock
import Control.Monad ((>=>))
import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified Data.Default as RDefault
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp


foreign import javascript interruptible
  "navigator.geolocation.getCurrentPosition(hs_good($c), hs_error($c), $1);"
  js_getCurrentPosition :: RTypes.JSRef (GeolocationOptions) -> IO (RInternal.JSEitherRef PositionError Position)
getCurrentPosition :: GeolocationOptions -> IO (Either PositionError Position)
getCurrentPosition arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_getCurrentPosition arg0'
  RInternal.fromJSEitherRef res

data Coordinates = Coordinates
  { latitude :: Maybe Double
  , longitude :: Maybe Double
  , altitude :: Maybe Double
  , accuracy :: Maybe Double
  , altitudeAccuracy :: Maybe Double
  , heading :: Maybe Double
  , speed :: Maybe Double
  } deriving (Eq, Ord, Show, Read)
instance RDefault.Default Coordinates where def = Coordinates RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def
instance RMarshal.ToJSRef Coordinates where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "latitude" latitude
    _setJust "longitude" longitude
    _setJust "altitude" altitude
    _setJust "accuracy" accuracy
    _setJust "altitudeAccuracy" altitudeAccuracy
    _setJust "heading" heading
    _setJust "speed" speed
    return obj
instance RMarshal.FromJSRef Coordinates where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "latitude" obj
    _x1 <- RInternal.fromProp "longitude" obj
    _x2 <- RInternal.fromProp "altitude" obj
    _x3 <- RInternal.fromProp "accuracy" obj
    _x4 <- RInternal.fromProp "altitudeAccuracy" obj
    _x5 <- RInternal.fromProp "heading" obj
    _x6 <- RInternal.fromProp "speed" obj
    return $ Coordinates RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2 RApp.<*> _x3 RApp.<*> _x4 RApp.<*> _x5 RApp.<*> _x6

data Position = Position
  { coords :: Coordinates
  , timestamp :: UTCTime
  } deriving (Eq, Ord, Show, Read)
instance RMarshal.ToJSRef Position where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "coords" coords
    _set "timestamp" timestamp
    return obj
instance RMarshal.FromJSRef Position where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "coords" obj
    _x1 <- RInternal.fromProp "timestamp" obj
    return $ Position RApp.<$> _x0 RApp.<*> _x1

data PositionError = PositionError
  { code :: PositionErrorCode
  , message :: String
  } deriving (Eq, Ord, Show, Read)
instance RMarshal.ToJSRef PositionError where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "code" code
    _set "message" message
    return obj
instance RMarshal.FromJSRef PositionError where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "code" obj
    _x1 <- RInternal.fromProp "message" obj
    return $ PositionError RApp.<$> _x0 RApp.<*> _x1

data PositionErrorCode
  = PermissionDenied
  | PositionUnavailable
  | Timeout
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "PositionError.PERMISSION_DENIED" _PositionErrorCode_PermissionDenied :: RTypes.JSRef PositionErrorCode
foreign import javascript unsafe "PositionError.POSITION_UNAVAILABLE" _PositionErrorCode_PositionUnavailable :: RTypes.JSRef PositionErrorCode
foreign import javascript unsafe "PositionError.TIMEOUT" _PositionErrorCode_Timeout :: RTypes.JSRef PositionErrorCode
instance RMarshal.ToJSRef PositionErrorCode where
  toJSRef PermissionDenied = return _PositionErrorCode_PermissionDenied
  toJSRef PositionUnavailable = return _PositionErrorCode_PositionUnavailable
  toJSRef Timeout = return _PositionErrorCode_Timeout
instance RMarshal.FromJSRef PositionErrorCode where
  fromJSRef = RInternal.js_fromEnum

data GeolocationOptions = GeolocationOptions
  { enableHighAccuracy :: Maybe Bool
  , timeout :: Maybe Double
  , maximumAge :: Maybe Double
  } deriving (Eq, Ord, Show, Read)
instance RDefault.Default GeolocationOptions where def = GeolocationOptions RDefault.def RDefault.def RDefault.def
instance RMarshal.ToJSRef GeolocationOptions where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "enableHighAccuracy" enableHighAccuracy
    _setJust "timeout" timeout
    _setJust "maximumAge" maximumAge
    return obj
instance RMarshal.FromJSRef GeolocationOptions where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "enableHighAccuracy" obj
    _x1 <- RInternal.fromProp "timeout" obj
    _x2 <- RInternal.fromProp "maximumAge" obj
    return $ GeolocationOptions RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2

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
