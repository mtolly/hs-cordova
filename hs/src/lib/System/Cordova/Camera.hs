
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

import GHCJS.Marshal
import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified Data.Default as RDefault
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp


foreign import javascript interruptible
  "navigator.camera.getPicture(hs_good($c), hs_error($c), $1);"
  js_getPicture :: RTypes.JSRef (CameraOptions) -> IO (RInternal.JSEitherRef (String) (String))
getPicture :: CameraOptions -> IO (Either (String) (String))
getPicture arg0 = do
  arg0' <- RMarshal.toJSRef arg0
  js_getPicture arg0' >>= RInternal.fromJSEitherRef

foreign import javascript interruptible
  "navigator.camera.cleanup(hs_good($c), hs_error($c));"
  js_cleanup ::  IO (RInternal.JSEitherRef (String) (()))
cleanup ::  IO (Either (String) (()))
cleanup  = do
  js_cleanup  >>= RInternal.fromJSEitherRef

data CameraOptions = CameraOptions { quality :: Maybe Int, destinationType :: Maybe DestinationType, sourceType :: Maybe SourceType, allowEdit :: Maybe Bool, encodingType :: Maybe EncodingType, targetWidth :: Maybe Int, targetHeight :: Maybe Int, mediaType :: Maybe MediaType, correctOrientation :: Maybe Bool, saveToPhotoAlbum :: Maybe Bool, popoverOptions :: Maybe PopoverOptions, cameraDirection :: Maybe Direction } deriving (Eq, Ord, Show, Read)
instance RDefault.Default CameraOptions where def = CameraOptions RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def
instance RMarshal.ToJSRef CameraOptions where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "quality" quality
    _setJust "destinationType" destinationType
    _setJust "sourceType" sourceType
    _setJust "allowEdit" allowEdit
    _setJust "encodingType" encodingType
    _setJust "targetWidth" targetWidth
    _setJust "targetHeight" targetHeight
    _setJust "mediaType" mediaType
    _setJust "correctOrientation" correctOrientation
    _setJust "saveToPhotoAlbum" saveToPhotoAlbum
    _setJust "popoverOptions" popoverOptions
    _setJust "cameraDirection" cameraDirection
    return obj
instance RMarshal.FromJSRef CameraOptions where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "quality" obj
    _x1 <- RInternal.fromProp "destinationType" obj
    _x2 <- RInternal.fromProp "sourceType" obj
    _x3 <- RInternal.fromProp "allowEdit" obj
    _x4 <- RInternal.fromProp "encodingType" obj
    _x5 <- RInternal.fromProp "targetWidth" obj
    _x6 <- RInternal.fromProp "targetHeight" obj
    _x7 <- RInternal.fromProp "mediaType" obj
    _x8 <- RInternal.fromProp "correctOrientation" obj
    _x9 <- RInternal.fromProp "saveToPhotoAlbum" obj
    _x10 <- RInternal.fromProp "popoverOptions" obj
    _x11 <- RInternal.fromProp "cameraDirection" obj
    return $ CameraOptions RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2 RApp.<*> _x3 RApp.<*> _x4 RApp.<*> _x5 RApp.<*> _x6 RApp.<*> _x7 RApp.<*> _x8 RApp.<*> _x9 RApp.<*> _x10 RApp.<*> _x11

data DestinationType = DataURL | FileURI | NativeURI deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.DestinationType.DATA_URL" _DestinationType_DataURL :: RTypes.JSRef DestinationType
foreign import javascript unsafe "Camera.DestinationType.FILE_URI" _DestinationType_FileURI :: RTypes.JSRef DestinationType
foreign import javascript unsafe "Camera.DestinationType.NATIVE_URI" _DestinationType_NativeURI :: RTypes.JSRef DestinationType
instance RMarshal.ToJSRef DestinationType where
  toJSRef DataURL = return _DestinationType_DataURL
  toJSRef FileURI = return _DestinationType_FileURI
  toJSRef NativeURI = return _DestinationType_NativeURI
instance RMarshal.FromJSRef DestinationType where
  fromJSRef = RInternal.js_fromEnum

data Direction = Back | Front deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.Direction.BACK" _Direction_Back :: RTypes.JSRef Direction
foreign import javascript unsafe "Camera.Direction.FRONT" _Direction_Front :: RTypes.JSRef Direction
instance RMarshal.ToJSRef Direction where
  toJSRef Back = return _Direction_Back
  toJSRef Front = return _Direction_Front
instance RMarshal.FromJSRef Direction where
  fromJSRef = RInternal.js_fromEnum

data EncodingType = JPEG | PNG deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.EncodingType.JPEG" _EncodingType_JPEG :: RTypes.JSRef EncodingType
foreign import javascript unsafe "Camera.EncodingType.PNG" _EncodingType_PNG :: RTypes.JSRef EncodingType
instance RMarshal.ToJSRef EncodingType where
  toJSRef JPEG = return _EncodingType_JPEG
  toJSRef PNG = return _EncodingType_PNG
instance RMarshal.FromJSRef EncodingType where
  fromJSRef = RInternal.js_fromEnum

data MediaType = Picture | Video | AllMedia deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.MediaType.PICTURE" _MediaType_Picture :: RTypes.JSRef MediaType
foreign import javascript unsafe "Camera.MediaType.VIDEO" _MediaType_Video :: RTypes.JSRef MediaType
foreign import javascript unsafe "Camera.MediaType.ALLMEDIA" _MediaType_AllMedia :: RTypes.JSRef MediaType
instance RMarshal.ToJSRef MediaType where
  toJSRef Picture = return _MediaType_Picture
  toJSRef Video = return _MediaType_Video
  toJSRef AllMedia = return _MediaType_AllMedia
instance RMarshal.FromJSRef MediaType where
  fromJSRef = RInternal.js_fromEnum

data PopoverArrowDirection = ArrowUp | ArrowDown | ArrowLeft | ArrowRight | ArrowAny deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_UP" _PopoverArrowDirection_ArrowUp :: RTypes.JSRef PopoverArrowDirection
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_DOWN" _PopoverArrowDirection_ArrowDown :: RTypes.JSRef PopoverArrowDirection
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_LEFT" _PopoverArrowDirection_ArrowLeft :: RTypes.JSRef PopoverArrowDirection
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_RIGHT" _PopoverArrowDirection_ArrowRight :: RTypes.JSRef PopoverArrowDirection
foreign import javascript unsafe "Camera.PopoverArrowDirection.ARROW_ANY" _PopoverArrowDirection_ArrowAny :: RTypes.JSRef PopoverArrowDirection
instance RMarshal.ToJSRef PopoverArrowDirection where
  toJSRef ArrowUp = return _PopoverArrowDirection_ArrowUp
  toJSRef ArrowDown = return _PopoverArrowDirection_ArrowDown
  toJSRef ArrowLeft = return _PopoverArrowDirection_ArrowLeft
  toJSRef ArrowRight = return _PopoverArrowDirection_ArrowRight
  toJSRef ArrowAny = return _PopoverArrowDirection_ArrowAny
instance RMarshal.FromJSRef PopoverArrowDirection where
  fromJSRef = RInternal.js_fromEnum

data PopoverOptions = PopoverOptions { popX :: Maybe Double, popY :: Maybe Double, popWidth :: Maybe Double, popHeight :: Maybe Double, popArrowDir :: Maybe PopoverArrowDirection } deriving (Eq, Ord, Show, Read)
instance RDefault.Default PopoverOptions where def = PopoverOptions RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def
instance RMarshal.ToJSRef PopoverOptions where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "x" popX
    _setJust "y" popY
    _setJust "width" popWidth
    _setJust "height" popHeight
    _setJust "arrowDir" popArrowDir
    return obj
instance RMarshal.FromJSRef PopoverOptions where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "x" obj
    _x1 <- RInternal.fromProp "y" obj
    _x2 <- RInternal.fromProp "width" obj
    _x3 <- RInternal.fromProp "height" obj
    _x4 <- RInternal.fromProp "arrowDir" obj
    return $ PopoverOptions RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2 RApp.<*> _x3 RApp.<*> _x4

data SourceType = PhotoLibrary | Camera | SavedPhotoAlbum deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.PictureSourceType.PHOTOLIBRARY" _SourceType_PhotoLibrary :: RTypes.JSRef SourceType
foreign import javascript unsafe "Camera.PictureSourceType.CAMERA" _SourceType_Camera :: RTypes.JSRef SourceType
foreign import javascript unsafe "Camera.PictureSourceType.SAVEDPHOTOALBUM" _SourceType_SavedPhotoAlbum :: RTypes.JSRef SourceType
instance RMarshal.ToJSRef SourceType where
  toJSRef PhotoLibrary = return _SourceType_PhotoLibrary
  toJSRef Camera = return _SourceType_Camera
  toJSRef SavedPhotoAlbum = return _SourceType_SavedPhotoAlbum
instance RMarshal.FromJSRef SourceType where
  fromJSRef = RInternal.js_fromEnum
