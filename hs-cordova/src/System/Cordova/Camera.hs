{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Camera where

import GHCJS.Types
import GHCJS.Foreign
import System.Cordova.Internal

foreign import javascript interruptible
  "navigator.camera.getPicture(hs_good($c), hs_error($c), { quality: $1, destinationType: Camera.DestinationType.DATA_URL });"
  js_getPicture :: Int -> IO (JSEither JSString JSString)

getPicture :: Int -> IO (Either String String)
getPicture q
  = fmap (either (Left . fromJSString) (Right . fromJSString))
  $ js_getPicture q >>= fromJSEither
