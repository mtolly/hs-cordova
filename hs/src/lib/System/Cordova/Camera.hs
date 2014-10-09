{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Camera where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import System.Cordova.Internal

data CameraOptions = CameraOptions
  { quality            :: Maybe Int -- ^ 0 to 100. Default is 50.
  , destinationType    :: Maybe DestinationType
  , sourceType         :: Maybe SourceType
  , allowEdit          :: Maybe Bool
  , encodingType       :: Maybe EncodingType
  , targetWidth        :: Maybe Int
  , targetHeight       :: Maybe Int
  , mediaType          :: Maybe MediaType
  , correctOrientation :: Maybe Bool
  , saveToPhotoAlbum   :: Maybe Bool
  -- , popoverOptions     ::
  , cameraDirection    :: Maybe Direction
  } deriving (Eq, Ord, Show, Read)

data DestinationType = DataURL | FileURI | NativeURI
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance ToJSRef DestinationType where
  toJSRef = getEnum ["Camera", "DestinationType"] cordovaEnum

data SourceType = PhotoLibrary | Camera | SavedPhotoAlbum
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance ToJSRef SourceType where
  toJSRef = getEnum ["Camera", "PictureSourceType"] cordovaEnum

data EncodingType = JPEG | PNG
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance ToJSRef EncodingType where
  toJSRef = getEnum ["Camera", "EncodingType"] cordovaEnum

data MediaType = Picture | Video | AllMedia
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance ToJSRef MediaType where
  toJSRef = getEnum ["Camera", "MediaType"] cordovaEnum

data Direction = Back | Front
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
instance ToJSRef Direction where
  toJSRef = getEnum ["Camera", "Direction"] cordovaEnum

cameraDef :: CameraOptions
cameraDef = CameraOptions
  { quality            = Nothing
  , destinationType    = Nothing
  , sourceType         = Nothing
  , allowEdit          = Nothing
  , encodingType       = Nothing
  , targetWidth        = Nothing
  , targetHeight       = Nothing
  , mediaType          = Nothing
  , correctOrientation = Nothing
  , saveToPhotoAlbum   = Nothing
  , cameraDirection    = Nothing
  }

instance ToJSRef CameraOptions where
  toJSRef opts = do
    obj <- newObj
    let setJust s f = case f opts of
          Nothing -> return ()
          Just x  -> toJSRef x >>= \ref -> setProp s ref obj
    setJust "quality"            quality
    setJust "destinationType"    destinationType
    setJust "sourceType"         sourceType
    setJust "allowEdit"          allowEdit
    setJust "encodingType"       encodingType
    setJust "targetWidth"        targetWidth
    setJust "targetHeight"       targetHeight
    setJust "mediaType"          mediaType
    setJust "correctOrientation" correctOrientation
    setJust "saveToPhotoAlbum"   saveToPhotoAlbum
    setJust "cameraDirection"    cameraDirection
    return obj

foreign import javascript interruptible
  "navigator.camera.getPicture(hs_good($c), hs_error($c), $1);"
  js_getPicture :: JSRef CameraOptions -> IO (JSEither JSString JSString)

getPicture :: CameraOptions -> IO (Either String String)
getPicture opts = do
  eitherJJ <- toJSRef opts >>= js_getPicture >>= fromJSEither
  return $ case eitherJJ of
    Left err -> Left $ fromJSString err
    Right x -> Right $ fromJSString x
