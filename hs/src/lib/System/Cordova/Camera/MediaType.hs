
module System.Cordova.Camera.MediaType where
import GHCJS.Types
import GHCJS.Marshal
import System.Cordova.Internal
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
