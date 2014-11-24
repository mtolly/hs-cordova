
module System.Cordova.Vibration
( vibrate
, vibrateFor
, vibratePattern
, vibrateCancel
) where

import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified System.Cordova.Internal as RInternal


foreign import javascript unsafe
  "navigator.vibrate();"
  js_vibrate ::  IO (RTypes.JSRef (()))
vibrate ::  IO (())
vibrate  =  do
  res <- js_vibrate 
  RInternal.fromJSRef' res
foreign import javascript unsafe
  "navigator.vibrate($1);"
  js_vibrateFor :: RTypes.JSRef (Int) -> IO (RTypes.JSRef (()))
vibrateFor :: Int -> IO (())
vibrateFor arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_vibrateFor arg0'
  RInternal.fromJSRef' res
foreign import javascript unsafe
  "navigator.vibrate($1);"
  js_vibratePattern :: RTypes.JSRef ([Int]) -> IO (RTypes.JSRef (()))
vibratePattern :: [Int] -> IO (())
vibratePattern arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_vibratePattern arg0'
  RInternal.fromJSRef' res

vibrateCancel :: IO ()
vibrateCancel = vibrateFor 0
