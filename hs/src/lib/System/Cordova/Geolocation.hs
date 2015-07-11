{-# LANGUAGE DataKinds #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
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
import GHCJS.Marshal
import Data.Time.Clock
import System.Cordova.EventListener
import System.Cordova.Binders
import qualified Data.Text as T
import Data.Default

jsRecord [d|
  data GeolocationOptions = GeolocationOptions
    { enableHighAccuracy :: Maybe Bool
    , timeout            :: Maybe Double
    , maximumAge         :: Maybe Double
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef, Default)
  |]

jsEnum "PositionError." [d|
  data PositionErrorCode
    = JS "PERMISSION_DENIED"    => PermissionDenied
    | JS "POSITION_UNAVAILABLE" => PositionUnavailable
    | JS "TIMEOUT"              => Timeout
    deriving (Eq, Ord, Show, Read, Enum, Bounded, ToJSRef, FromJSRef)
  |]

jsRecord [d|
  data PositionError = PositionError
    { code    :: PositionErrorCode
    , message :: T.Text
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef)
  |]

jsRecord [d|
  data Coordinates = Coordinates
    { latitude         :: Maybe Double
    , longitude        :: Maybe Double
    , altitude         :: Maybe Double
    , accuracy         :: Maybe Double
    , altitudeAccuracy :: Maybe Double
    , heading          :: Maybe Double
    , speed            :: Maybe Double
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef, Default)
  |]

jsRecord [d|
  data Position = Position
    { coords    :: Coordinates
    , timestamp :: UTCTime
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef)
  |]

jsImport [d|
  getCurrentPosition :: GeolocationOptions -> IO (Either PositionError Position)
  getCurrentPosition = "navigator.geolocation.getCurrentPosition(hs_good($c), hs_error($c), $1);"
  |]

foreign import javascript unsafe
  "navigator.geolocation.watchPosition($2, $3, $1)"
  js_watchPosition
  :: JSRef GeolocationOptions
  -> JSFun (JSRef Position -> IO ())
  -> JSFun (JSRef PositionError -> IO ())
  -> IO (JSRef String)

foreign import javascript unsafe
  "navigator.geolocation.clearWatch($1);"
  js_clearWatch :: JSRef String -> IO ()

watchPosition
  :: GeolocationOptions
  -> (Either PositionError Position -> IO ())
  -> IO (IO ())
watchPosition opts f = do
  opts' <- toJSRef opts
  globalListener (js_watchPosition opts') js_clearWatch f
