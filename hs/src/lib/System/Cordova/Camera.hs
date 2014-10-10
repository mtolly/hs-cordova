{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Camera where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import System.Cordova.Internal
import System.Cordova.Base



data CameraOptions = CameraOptions { quality :: Maybe Int, destinationType :: Maybe DestinationType, sourceType :: Maybe SourceType, allowEdit :: Maybe Bool, encodingType :: Maybe EncodingType, targetWidth :: Maybe Int, targetHeight :: Maybe Int, mediaType :: Maybe MediaType, correctOrientation :: Maybe Bool, saveToPhotoAlbum :: Maybe Bool, popoverOptions :: Maybe PopoverOptions, cameraDirection :: Maybe Direction } deriving (Eq, Ord, Show, Read)
instance Default CameraOptions where defaultValue = CameraOptions Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
instance ToJSRef CameraOptions where
  toJSRef opts = do
    obj <- newObj
    let setJust s f = case f opts of
          Nothing -> return ()
          Just x -> toJSRef x >>= \ref -> setProp s ref obj
    setJust "quality" quality
    setJust "destinationType" destinationType
    setJust "sourceType" sourceType
    setJust "allowEdit" allowEdit
    setJust "encodingType" encodingType
    setJust "targetWidth" targetWidth
    setJust "targetHeight" targetHeight
    setJust "mediaType" mediaType
    setJust "correctOrientation" correctOrientation
    setJust "saveToPhotoAlbum" saveToPhotoAlbum
    setJust "popoverOptions" popoverOptions
    setJust "cameraDirection" cameraDirection
    return obj

data DestinationType = DataURL | FileURI | NativeURI deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.DestinationType.DATA_URL" _DestinationType_DataURL :: JSRef DestinationType
foreign import javascript unsafe "Camera.DestinationType.FILE_URI" _DestinationType_FileURI :: JSRef DestinationType
foreign import javascript unsafe "Camera.DestinationType.NATIVE_URI" _DestinationType_NativeURI :: JSRef DestinationType
instance ToJSRef DestinationType where
  toJSRef DataURL = return _DestinationType_DataURL
  toJSRef FileURI = return _DestinationType_FileURI
  toJSRef NativeURI = return _DestinationType_NativeURI

data SourceType = PhotoLibrary | Camera | SavedPhotoAlbum deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.PictureSourceType.PHOTOLIBRARY" _SourceType_PhotoLibrary :: JSRef SourceType
foreign import javascript unsafe "Camera.PictureSourceType.CAMERA" _SourceType_Camera :: JSRef SourceType
foreign import javascript unsafe "Camera.PictureSourceType.SAVEDPHOTOALBUM" _SourceType_SavedPhotoAlbum :: JSRef SourceType
instance ToJSRef SourceType where
  toJSRef PhotoLibrary = return _SourceType_PhotoLibrary
  toJSRef Camera = return _SourceType_Camera
  toJSRef SavedPhotoAlbum = return _SourceType_SavedPhotoAlbum

data EncodingType = JPEG | PNG deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.EncodingType.JPEG" _EncodingType_JPEG :: JSRef EncodingType
foreign import javascript unsafe "Camera.EncodingType.PNG" _EncodingType_PNG :: JSRef EncodingType
instance ToJSRef EncodingType where
  toJSRef JPEG = return _EncodingType_JPEG
  toJSRef PNG = return _EncodingType_PNG

data MediaType = Picture | Video | AllMedia deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.MediaType.PICTURE" _MediaType_Picture :: JSRef MediaType
foreign import javascript unsafe "Camera.MediaType.VIDEO" _MediaType_Video :: JSRef MediaType
foreign import javascript unsafe "Camera.MediaType.ALLMEDIA" _MediaType_AllMedia :: JSRef MediaType
instance ToJSRef MediaType where
  toJSRef Picture = return _MediaType_Picture
  toJSRef Video = return _MediaType_Video
  toJSRef AllMedia = return _MediaType_AllMedia

data PopoverOptions = PopoverOptions { popX :: Maybe Int, popY :: Maybe Int, popWidth :: Maybe Int, popHeight :: Maybe Int, arrowDir :: Maybe Int } deriving (Eq, Ord, Show, Read)
instance Default PopoverOptions where defaultValue = PopoverOptions Nothing Nothing Nothing Nothing Nothing
instance ToJSRef PopoverOptions where
  toJSRef opts = do
    obj <- newObj
    let setJust s f = case f opts of
          Nothing -> return ()
          Just x -> toJSRef x >>= \ref -> setProp s ref obj
    setJust "x" popX
    setJust "y" popY
    setJust "width" popWidth
    setJust "height" popHeight
    setJust "PopoverArrowDirection" arrowDir
    return obj

data PopoverArrowDirection = ArrowUp | ArrowDown | ArrowLeft | ArrowRight | ArrowAny deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_UP" _PopoverArrowDirection_ArrowUp :: JSRef PopoverArrowDirection
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_DOWN" _PopoverArrowDirection_ArrowDown :: JSRef PopoverArrowDirection
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_LEFT" _PopoverArrowDirection_ArrowLeft :: JSRef PopoverArrowDirection
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_RIGHT" _PopoverArrowDirection_ArrowRight :: JSRef PopoverArrowDirection
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_ANY" _PopoverArrowDirection_ArrowAny :: JSRef PopoverArrowDirection
instance ToJSRef PopoverArrowDirection where
  toJSRef ArrowUp = return _PopoverArrowDirection_ArrowUp
  toJSRef ArrowDown = return _PopoverArrowDirection_ArrowDown
  toJSRef ArrowLeft = return _PopoverArrowDirection_ArrowLeft
  toJSRef ArrowRight = return _PopoverArrowDirection_ArrowRight
  toJSRef ArrowAny = return _PopoverArrowDirection_ArrowAny

data Direction = Back | Front deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.Direction.BACK" _Direction_Back :: JSRef Direction
foreign import javascript unsafe "Camera.Direction.FRONT" _Direction_Front :: JSRef Direction
instance ToJSRef Direction where
  toJSRef Back = return _Direction_Back
  toJSRef Front = return _Direction_Front

foreign import javascript interruptible
  "navigator.camera.getPicture(hs_good($c), hs_error($c), $1);"
  js_getPicture :: JSRef CameraOptions -> IO (JSEither JSString JSString)

getPicture :: CameraOptions -> IO (Either String String)
getPicture opts = do
  eitherJJ <- toJSRef opts >>= js_getPicture >>= fromJSEither
  return $ case eitherJJ of
    Left err -> Left $ fromJSString err
    Right x -> Right $ fromJSString x

foreign import javascript interruptible
  "navigator.camera.cleanup(hs_good($c), hs_error($c));"
  js_cleanup :: IO (JSEither JSString (JSRef ()))

cleanup :: IO (Maybe String)
cleanup = js_cleanup >>= fromJSEither >>= \x -> return $ case x of
  Left err -> Just $ fromJSString err
  Right _  -> Nothing
