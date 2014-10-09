{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Camera where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import System.Cordova.Internal
import Data.Char (toUpper)

foreign import javascript unsafe "window.Camera"
  windowCamera :: JSRef ()

data CameraOptions = CameraOptions
  { quality         :: Maybe Int -- ^ 0 to 100. Default is 50.
  , destinationType :: Maybe DestinationType
  , sourceType      :: Maybe SourceType
  } deriving (Eq, Ord, Show, Read)

data DestinationType = DataURL | FileURI | NativeURI
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToJSRef DestinationType where
  toJSRef dst = let
    str = case dst of
      DataURL -> "DATA_URL"
      FileURI -> "FILE_URI"
      NativeURI -> "NATIVE_URI"
    in getProp "DestinationType" windowCamera >>= getProp str

data SourceType = PhotoLibrary | Camera | SavedPhotoAlbum
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToJSRef SourceType where
  toJSRef src = let
    str = map toUpper $ show src
    in getProp "PictureSourceType" windowCamera >>= getProp str

cameraDef :: CameraOptions
cameraDef = CameraOptions
  { quality         = Nothing
  , destinationType = Nothing
  , sourceType      = Nothing
  }

instance ToJSRef CameraOptions where
  toJSRef opts = do
    obj <- newObj
    let setJust s f = case f opts of
          Nothing -> return ()
          Just x  -> toJSRef x >>= \ref -> setProp s ref obj
    setJust "quality"         quality
    setJust "destinationType" destinationType
    setJust "sourceType"      sourceType
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
