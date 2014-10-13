{-# LANGUAGE JavaScriptFFI #-}
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

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import System.Cordova.Internal
import Data.Default
import Control.Applicative



data CameraOptions = CameraOptions { quality :: Maybe Int, destinationType :: Maybe DestinationType, sourceType :: Maybe SourceType, allowEdit :: Maybe Bool, encodingType :: Maybe EncodingType, targetWidth :: Maybe Int, targetHeight :: Maybe Int, mediaType :: Maybe MediaType, correctOrientation :: Maybe Bool, saveToPhotoAlbum :: Maybe Bool, popoverOptions :: Maybe PopoverOptions, cameraDirection :: Maybe Direction } deriving (Eq, Ord, Show, Read)
instance Default CameraOptions where def = CameraOptions def def def def def def def def def def def def
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
instance FromJSRef CameraOptions where
  fromJSRef obj = do
    _x0 <- fromProp "quality" obj
    _x1 <- fromProp "destinationType" obj
    _x2 <- fromProp "sourceType" obj
    _x3 <- fromProp "allowEdit" obj
    _x4 <- fromProp "encodingType" obj
    _x5 <- fromProp "targetWidth" obj
    _x6 <- fromProp "targetHeight" obj
    _x7 <- fromProp "mediaType" obj
    _x8 <- fromProp "correctOrientation" obj
    _x9 <- fromProp "saveToPhotoAlbum" obj
    _x10 <- fromProp "popoverOptions" obj
    _x11 <- fromProp "cameraDirection" obj
    return $ CameraOptions <$> _x0 <*> _x1 <*> _x2 <*> _x3 <*> _x4 <*> _x5 <*> _x6 <*> _x7 <*> _x8 <*> _x9 <*> _x10 <*> _x11

data DestinationType = DataURL | FileURI | NativeURI deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.DestinationType.DATA_URL" _DestinationType_DataURL :: JSRef DestinationType
foreign import javascript unsafe "Camera.DestinationType.FILE_URI" _DestinationType_FileURI :: JSRef DestinationType
foreign import javascript unsafe "Camera.DestinationType.NATIVE_URI" _DestinationType_NativeURI :: JSRef DestinationType
instance ToJSRef DestinationType where
  toJSRef DataURL = return _DestinationType_DataURL
  toJSRef FileURI = return _DestinationType_FileURI
  toJSRef NativeURI = return _DestinationType_NativeURI
instance FromJSRef DestinationType where
  fromJSRef = js_fromEnum

data SourceType = PhotoLibrary | Camera | SavedPhotoAlbum deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.PictureSourceType.PHOTOLIBRARY" _SourceType_PhotoLibrary :: JSRef SourceType
foreign import javascript unsafe "Camera.PictureSourceType.CAMERA" _SourceType_Camera :: JSRef SourceType
foreign import javascript unsafe "Camera.PictureSourceType.SAVEDPHOTOALBUM" _SourceType_SavedPhotoAlbum :: JSRef SourceType
instance ToJSRef SourceType where
  toJSRef PhotoLibrary = return _SourceType_PhotoLibrary
  toJSRef Camera = return _SourceType_Camera
  toJSRef SavedPhotoAlbum = return _SourceType_SavedPhotoAlbum
instance FromJSRef SourceType where
  fromJSRef = js_fromEnum

data EncodingType = JPEG | PNG deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.EncodingType.JPEG" _EncodingType_JPEG :: JSRef EncodingType
foreign import javascript unsafe "Camera.EncodingType.PNG" _EncodingType_PNG :: JSRef EncodingType
instance ToJSRef EncodingType where
  toJSRef JPEG = return _EncodingType_JPEG
  toJSRef PNG = return _EncodingType_PNG
instance FromJSRef EncodingType where
  fromJSRef = js_fromEnum

data MediaType = Picture | Video | AllMedia deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.MediaType.PICTURE" _MediaType_Picture :: JSRef MediaType
foreign import javascript unsafe "Camera.MediaType.VIDEO" _MediaType_Video :: JSRef MediaType
foreign import javascript unsafe "Camera.MediaType.ALLMEDIA" _MediaType_AllMedia :: JSRef MediaType
instance ToJSRef MediaType where
  toJSRef Picture = return _MediaType_Picture
  toJSRef Video = return _MediaType_Video
  toJSRef AllMedia = return _MediaType_AllMedia
instance FromJSRef MediaType where
  fromJSRef = js_fromEnum

data PopoverOptions = PopoverOptions { popX :: Maybe Int, popY :: Maybe Int, popWidth :: Maybe Int, popHeight :: Maybe Int, arrowDir :: Maybe Int } deriving (Eq, Ord, Show, Read)
instance Default PopoverOptions where def = PopoverOptions def def def def def
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
instance FromJSRef PopoverOptions where
  fromJSRef obj = do
    _x0 <- fromProp "x" obj
    _x1 <- fromProp "y" obj
    _x2 <- fromProp "width" obj
    _x3 <- fromProp "height" obj
    _x4 <- fromProp "PopoverArrowDirection" obj
    return $ PopoverOptions <$> _x0 <*> _x1 <*> _x2 <*> _x3 <*> _x4

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
instance FromJSRef PopoverArrowDirection where
  fromJSRef = js_fromEnum

data Direction = Back | Front deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.Direction.BACK" _Direction_Back :: JSRef Direction
foreign import javascript unsafe "Camera.Direction.FRONT" _Direction_Front :: JSRef Direction
instance ToJSRef Direction where
  toJSRef Back = return _Direction_Back
  toJSRef Front = return _Direction_Front
instance FromJSRef Direction where
  fromJSRef = js_fromEnum

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
