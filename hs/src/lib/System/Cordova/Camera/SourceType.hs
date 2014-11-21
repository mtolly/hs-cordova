
module System.Cordova.Camera.SourceType where
import GHCJS.Types
import GHCJS.Marshal
import System.Cordova.Internal
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
