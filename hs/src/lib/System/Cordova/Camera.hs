{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
module System.Cordova.Camera
( CameraOptions(..)
, DestinationType(..)
, SourceType(..)
, EncodingType(..)
, MediaType(..)
, PopoverOptions(..)
, PopoverArrowDirection(..)
, Direction(..)
, getPicture
, cleanup
) where

import qualified Data.Text as T
import System.Cordova.Binders
import Data.Default
import GHCJS.Marshal

jsEnum "Camera.DestinationType." [d|
  data DestinationType
    = JS "DATA_URL"   => DataURL
    | JS "FILE_URI"   => FileURI
    | JS "NATIVE_URI" => NativeURI
    deriving (Eq, Ord, Show, Read, Enum, Bounded, ToJSRef, FromJSRef)
  |]

jsEnum "Camera.Direction." [d|
  data Direction = Back | Front
    deriving (Eq, Ord, Show, Read, Enum, Bounded, ToJSRef, FromJSRef)
  |]

jsEnum "Camera.EncodingType." [d|
  data EncodingType = JPEG | PNG
    deriving (Eq, Ord, Show, Read, Enum, Bounded, ToJSRef, FromJSRef)
  |]

jsEnum "Camera.MediaType." [d|
  data MediaType = Picture | Video | AllMedia
    deriving (Eq, Ord, Show, Read, Enum, Bounded, ToJSRef, FromJSRef)
  |]

jsEnum "Camera.PictureSourceType." [d|
  data SourceType = PhotoLibrary | Camera | SavedPhotoAlbum
    deriving (Eq, Ord, Show, Read, Enum, Bounded, ToJSRef, FromJSRef)
  |]

jsEnum "Camera.PopoverArrowDirection." [d|
  data PopoverArrowDirection
    = JS "ARROW_UP"    => ArrowUp
    | JS "ARROW_DOWN"  => ArrowDown
    | JS "ARROW_LEFT"  => ArrowLeft
    | JS "ARROW_RIGHT" => ArrowRight
    | JS "ARROW_ANY"   => ArrowAny
    deriving (Eq, Ord, Show, Read, Enum, Bounded, ToJSRef, FromJSRef)
  |]

jsRecord [d|
  data PopoverOptions = PopoverOptions
    { popX :: JS "x" => Maybe Double
    , popY :: JS "y" => Maybe Double
    , popWidth :: JS "width" => Maybe Double
    , popHeight :: JS "height" => Maybe Double
    , popArrowDir :: JS "arrowDir" => Maybe PopoverArrowDirection
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef, Default)
  |]

jsRecord [d|
  data CameraOptions = CameraOptions
    { quality            :: Maybe Int
    , destinationType    :: Maybe DestinationType
    , sourceType         :: Maybe SourceType
    , allowEdit          :: Maybe Bool
    , encodingType       :: Maybe EncodingType
    , targetWidth        :: Maybe Int
    , targetHeight       :: Maybe Int
    , mediaType          :: Maybe MediaType
    , correctOrientation :: Maybe Bool
    , saveToPhotoAlbum   :: Maybe Bool
    , popoverOptions     :: Maybe PopoverOptions
    , cameraDirection    :: Maybe Direction
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef, Default)
  |]

jsImport [d|
  getPicture :: CameraOptions -> IO (Either T.Text T.Text)
  getPicture = jsCode "navigator.camera.getPicture(hs_good($c), hs_error($c), $1);"
  |]

jsImport [d|
  cleanup :: IO (Either T.Text ())
  cleanup = jsCode "navigator.camera.cleanup(hs_good($c), hs_error($c));"
  |]
