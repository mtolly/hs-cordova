
module System.Cordova.BatteryStatus
( Status(..)
, onStatus
, onCritical
, onLow
) where

import System.Cordova.EventListener
import Control.Monad ((>=>))
import System.Cordova.Internal (fromJSRef')
import qualified GHCJS.Marshal as RMarshal
import qualified Data.Default as RDefault
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp


data Status = Status
  { level :: Maybe Double
  , isPlugged :: Maybe Bool
  } deriving (Eq, Ord, Show, Read)
instance RDefault.Default Status where def = Status RDefault.def RDefault.def
instance RMarshal.ToJSRef Status where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "level" level
    _setJust "isPlugged" isPlugged
    return obj
instance RMarshal.FromJSRef Status where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "level" obj
    _x1 <- RInternal.fromProp "isPlugged" obj
    return $ Status RApp.<$> _x0 RApp.<*> _x1

onStatus, onCritical, onLow :: (Status -> IO ()) -> IO (IO ())
onStatus   f = addEventListener1 "batterystatus"   (fromJSRef' >=> f) window
onCritical f = addEventListener1 "batterycritical" (fromJSRef' >=> f) window
onLow      f = addEventListener1 "batterylow"      (fromJSRef' >=> f) window
