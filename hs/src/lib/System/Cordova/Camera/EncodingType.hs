
module System.Cordova.Camera.EncodingType where
import GHCJS.Types
import GHCJS.Marshal
import System.Cordova.Internal
data EncodingType = JPEG | PNG deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.EncodingType.JPEG" _EncodingType_JPEG :: JSRef EncodingType
foreign import javascript unsafe "Camera.EncodingType.PNG" _EncodingType_PNG :: JSRef EncodingType
instance ToJSRef EncodingType where
  toJSRef JPEG = return _EncodingType_JPEG
  toJSRef PNG = return _EncodingType_PNG
instance FromJSRef EncodingType where
  fromJSRef = js_fromEnum
