
module System.Cordova.Camera.DestinationType where
import GHCJS.Types
import GHCJS.Marshal
import System.Cordova.Internal
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
