{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.DeviceMotion
( Acceleration(..)
, AccelerometerOptions(..)
, getCurrentAcceleration
, watchAcceleration
) where

import GHCJS.Types
import GHCJS.Marshal
import Data.Time
import System.Cordova.EventListener

jsImport "navigator.accelerometer.getCurrentAcceleration(hs_good($c), hs_error($c));"
  [t| IO (Either () Acceleration) |]

jsRecord [d|
  data Acceleration = Acceleration
    { accX      :: JS "x" => Maybe Double
    , accY      :: JS "y" => Maybe Double
    , accZ      :: JS "z" => Maybe Double
    , timestamp ::           Maybe UTCTime
    } deriving (Eq, Ord, Show, Read, FromJSRef)
  |]

jsRecord [d|
  data AccelerometerOptions = AccelerometerOptions
    { frequency :: Maybe Double -- ^ TODO: should this be period?
    }
  |]

foreign import javascript unsafe
  "navigator.accelerometer.watchAcceleration($2, $3, $1)"
  js_watchAcceleration
  :: JSRef AccelerometerOptions
  -> JSFun (JSRef Acceleration -> IO ())
  -> JSFun (JSRef () -> IO ())
  -> IO (JSRef String)

foreign import javascript unsafe
  "navigator.accelerometer.clearWatch($1);"
  js_clearWatch :: JSRef String -> IO ()

watchAcceleration
  :: AccelerometerOptions
  -> (Either () Acceleration -> IO ())
  -> IO (IO ())
watchAcceleration opts f = do
  opts' <- toJSRef opts
  globalListener (js_watchAcceleration opts') js_clearWatch f
