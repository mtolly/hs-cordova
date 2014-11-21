
module System.Cordova.Camera.PopoverArrowDirection where
import GHCJS.Types
import GHCJS.Marshal
import System.Cordova.Internal
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
