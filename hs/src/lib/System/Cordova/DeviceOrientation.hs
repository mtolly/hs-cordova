
{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.DeviceOrientation
( CompassHeading(..)
, CompassError(..)
, CompassErrorCode(..)
, getCurrentHeading
, watchHeading
) where

import GHCJS.Types
import GHCJS.Marshal
import Data.Time.Clock
import System.Cordova.EventListener
import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified Data.Default as RDefault
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp


foreign import javascript interruptible
  "navigator.compass.getCurrentHeading(hs_good($c), hs_error($c));"
  js_getCurrentHeading ::  IO (RInternal.JSEitherRef CompassError CompassHeading)
getCurrentHeading ::  IO (Either CompassError CompassHeading)
getCurrentHeading  =  do
  res <- js_getCurrentHeading 
  RInternal.fromJSEitherRef res

data CompassHeading = CompassHeading
  { magneticHeading :: Maybe Double
  , trueHeading :: Maybe Double
  , headingAccuracy :: Maybe Double
  , timestamp :: Maybe UTCTime
  } deriving (Eq, Ord, Show, Read)
instance RDefault.Default CompassHeading where def = CompassHeading RDefault.def RDefault.def RDefault.def RDefault.def
instance RMarshal.FromJSRef CompassHeading where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "magneticHeading" obj
    _x1 <- RInternal.fromProp "trueHeading" obj
    _x2 <- RInternal.fromProp "headingAccuracy" obj
    _x3 <- RInternal.fromProp "timestamp" obj
    return $ CompassHeading RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2 RApp.<*> _x3

data CompassError = CompassError
  { code :: CompassErrorCode
  } deriving (Eq, Ord, Show, Read)
instance RMarshal.ToJSRef CompassError where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "code" code
    return obj
instance RMarshal.FromJSRef CompassError where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "code" obj
    return $ CompassError RApp.<$> _x0

data CompassErrorCode
  = CompassInternalErr
  | CompassNotSupported
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "CompassError.COMPASS_INTERNAL_ERR" _CompassErrorCode_CompassInternalErr :: RTypes.JSRef CompassErrorCode
foreign import javascript unsafe "CompassError.COMPASS_NOT_SUPPORTED" _CompassErrorCode_CompassNotSupported :: RTypes.JSRef CompassErrorCode
instance RMarshal.ToJSRef CompassErrorCode where
  toJSRef CompassInternalErr = return _CompassErrorCode_CompassInternalErr
  toJSRef CompassNotSupported = return _CompassErrorCode_CompassNotSupported
instance RMarshal.FromJSRef CompassErrorCode where
  fromJSRef = RInternal.js_fromEnum

data CompassOptions = CompassOptions
  { frequency :: Maybe Double
  , watchFilter :: Maybe Double
  } deriving (Eq, Ord, Show, Read)
instance RDefault.Default CompassOptions where def = CompassOptions RDefault.def RDefault.def
instance RMarshal.ToJSRef CompassOptions where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "frequency" frequency
    _setJust "filter" watchFilter
    return obj
instance RMarshal.FromJSRef CompassOptions where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "frequency" obj
    _x1 <- RInternal.fromProp "filter" obj
    return $ CompassOptions RApp.<$> _x0 RApp.<*> _x1

foreign import javascript unsafe
  "navigator.compass.watchHeading($2, $3, $1)"
  js_watchHeading
  :: JSRef CompassOptions
  -> JSFun (JSRef CompassHeading -> IO ())
  -> JSFun (JSRef CompassError -> IO ())
  -> IO (JSRef String)

foreign import javascript unsafe
  "navigator.compass.clearWatch($1);"
  js_clearWatch :: JSRef String -> IO ()

watchHeading
  :: CompassOptions
  -> (Either CompassError CompassHeading -> IO ())
  -> IO (IO ())
watchHeading opts f = do
  opts' <- toJSRef opts
  globalListener (js_watchHeading opts') js_clearWatch f
