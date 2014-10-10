module System.Cordova.Vibrate
( vibrate
, vibrateFor
, vibratePattern
, vibrateCancel
) where

import GHCJS.Types (JSRef)
import GHCJS.Marshal (toJSRef)

foreign import javascript unsafe
  "navigator.vibrate();"
  vibrate :: IO ()

foreign import javascript unsafe
  "navigator.vibrate($1);"
  vibrateFor :: Int -> IO ()

foreign import javascript unsafe
  "navigator.vibrate($1);"
  js_vibratePattern :: JSRef [Int] -> IO ()

vibratePattern :: [Int] -> IO ()
vibratePattern pat = toJSRef pat >>= js_vibratePattern

vibrateCancel :: IO ()
vibrateCancel = vibrateFor 0
