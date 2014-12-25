
module System.Cordova.Vibration
( vibrate
, vibrateCancel
) where

import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified System.Cordova.Internal as RInternal


foreign import javascript unsafe
  "navigator.vibrate($1);"
  js_vibrate :: RTypes.JSRef ([Int]) -> IO (RTypes.JSRef (()))
vibrate :: [Int] -> IO (())
vibrate arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_vibrate arg0'
  RInternal.fromJSRef' res
-- no arg form is deprecated.
-- single int arg is equivalent to [int].

vibrateCancel :: IO ()
vibrateCancel = vibrate []
