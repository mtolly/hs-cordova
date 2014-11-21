
module System.Cordova.FileSystem.Storage where
import GHCJS.Types
import GHCJS.Marshal
import System.Cordova.Internal
data Storage = Temporary | Persistent deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "window.TEMPORARY" _Storage_Temporary :: JSRef Storage
foreign import javascript unsafe "window.PERSISTENT" _Storage_Persistent :: JSRef Storage
instance ToJSRef Storage where
  toJSRef Temporary = return _Storage_Temporary
  toJSRef Persistent = return _Storage_Persistent
instance FromJSRef Storage where
  fromJSRef = js_fromEnum
