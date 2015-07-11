{-# LANGUAGE TemplateHaskell #-}
module System.Cordova.BatteryStatus
( Status(..)
, onStatus
, onCritical
, onLow
) where

import System.Cordova.EventListener
import System.Cordova.Binders
import Control.Monad ((>=>))
import System.Cordova.Internal (fromJSRef')
import qualified Data.Text as T
import Data.Default
import GHCJS.Marshal

jsRecord [d|
  data Status = Status
    { level     :: Maybe Double
    , isPlugged :: Maybe Bool
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef, Default)
  |]

onStatus, onCritical, onLow :: (Status -> IO ()) -> IO (IO ())
onStatus   f = addEventListener1 (T.pack "batterystatus"  ) (fromJSRef' >=> f) window
onCritical f = addEventListener1 (T.pack "batterycritical") (fromJSRef' >=> f) window
onLow      f = addEventListener1 (T.pack "batterylow"     ) (fromJSRef' >=> f) window
