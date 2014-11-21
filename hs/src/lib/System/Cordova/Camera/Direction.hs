
module System.Cordova.Camera.Direction where
import GHCJS.Types
import GHCJS.Marshal
import System.Cordova.Internal
data Direction = Back | Front deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Camera.Direction.BACK" _Direction_Back :: JSRef Direction
foreign import javascript unsafe "Camera.Direction.FRONT" _Direction_Front :: JSRef Direction
instance ToJSRef Direction where
  toJSRef Back = return _Direction_Back
  toJSRef Front = return _Direction_Front
instance FromJSRef Direction where
  fromJSRef = js_fromEnum
