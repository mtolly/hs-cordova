
{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.DeviceMotion
( Acceleration(..)
, AccelerometerOptions(..)
, getCurrentAcceleration
, watchAcceleration
) where

import GHCJS.Types
import GHCJS.Marshal
import Data.Time.Clock
import System.Cordova.EventListener
import qualified GHCJS.Marshal as RMarshal
import qualified Data.Default as RDefault
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp
import qualified Data.Text as RText


foreign import javascript interruptible
  "navigator.accelerometer.getCurrentAcceleration(hs_good($c), hs_error($c));"
  js_getCurrentAcceleration ::  IO (RInternal.JSEitherRef () Acceleration)
getCurrentAcceleration ::  IO (Either () Acceleration)
getCurrentAcceleration  =  do
  res <- js_getCurrentAcceleration 
  RInternal.fromJSEitherRef res

data Acceleration = Acceleration
  { accX :: Maybe Double
  , accY :: Maybe Double
  , accZ :: Maybe Double
  , timestamp :: Maybe UTCTime
  } deriving (Eq, Ord, Show, Read)
instance  RDefault.Default (Acceleration) where def = Acceleration RDefault.def RDefault.def RDefault.def RDefault.def
instance  RMarshal.FromJSRef (Acceleration) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "x") obj
    _x1 <- RInternal.fromProp (RText.pack "y") obj
    _x2 <- RInternal.fromProp (RText.pack "z") obj
    _x3 <- RInternal.fromProp (RText.pack "timestamp") obj
    return $ Acceleration RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2 RApp.<*> _x3

newtype AccelerometerOptions = AccelerometerOptions
  { frequency :: Maybe Double
  } deriving (Eq, Ord, Show, Read)
instance  RDefault.Default (AccelerometerOptions) where def = AccelerometerOptions RDefault.def
instance  RMarshal.ToJSRef (AccelerometerOptions) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "frequency" frequency
    return obj
instance  RMarshal.FromJSRef (AccelerometerOptions) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "frequency") obj
    return $ AccelerometerOptions RApp.<$> _x0

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
