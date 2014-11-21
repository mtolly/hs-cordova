
module System.Cordova.Camera.CameraOptions where
import GHCJS.Foreign
import GHCJS.Marshal
import Data.Default
import Control.Applicative
import System.Cordova.Internal
import System.Cordova.Camera.DestinationType
import System.Cordova.Camera.SourceType
import System.Cordova.Camera.EncodingType
import System.Cordova.Camera.MediaType
import System.Cordova.Camera.PopoverOptions
import System.Cordova.Camera.Direction
data CameraOptions = CameraOptions { quality :: Maybe Int, destinationType :: Maybe DestinationType, sourceType :: Maybe SourceType, allowEdit :: Maybe Bool, encodingType :: Maybe EncodingType, targetWidth :: Maybe Int, targetHeight :: Maybe Int, mediaType :: Maybe MediaType, correctOrientation :: Maybe Bool, saveToPhotoAlbum :: Maybe Bool, popoverOptions :: Maybe PopoverOptions, cameraDirection :: Maybe Direction } deriving (Eq, Ord, Show, Read)
instance Default CameraOptions where def = CameraOptions def def def def def def def def def def def def
instance ToJSRef CameraOptions where
  toJSRef opts = do
    obj <- newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> toJSRef x >>= \ref -> setProp s ref obj
        _set s f = toJSRef (f opts) >>= \ref -> setProp s ref obj
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
