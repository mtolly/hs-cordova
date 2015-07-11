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

jsImport "navigator.compass.getCurrentHeading(hs_good($c), hs_error($c));"
  [t| IO (Either CompassError CompassHeading) |]

jsRecord [d|
  data CompassHeading = CompassHeading
    { magneticHeading :: Maybe Double
    , trueHeading     :: Maybe Double
    , headingAccuracy :: Maybe Double
    , timestamp       :: Maybe UTCTime
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef)
  |]

jsRecord [d|
  data CompassError = CompassError
    { code :: CompassErrorCode
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef)
  |]

jsEnum "CompassError." [d|
  data CompassErrorCode
    = JS "COMPASS_INTERNAL_ERR"  => CompassInternalErr
    | JS "COMPASS_NOT_SUPPORTED" => CompassNotSupported
  |]

jsRecord [d|
  data CompassOptions = CompassOptions
    { frequency   ::                Maybe Double
    , watchFilter :: JS "filter" => Maybe Double
    }
  |]

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
