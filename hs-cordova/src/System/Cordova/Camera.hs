{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Camera where

import GHCJS.Types
import GHCJS.Foreign

foreign import javascript interruptible
  "navigator.camera.getPicture(function(url) { $c('.' + url); }, function(err) { $c('!' + url); }, { quality: $1, destinationType: Camera.DestinationType.DATA_URL });"
  js_getPicture :: Int -> IO JSString

getPicture :: Int -> IO (Either String String)
getPicture q = do
  s <- js_getPicture q
  case fromJSString s of
    '.' : url -> return $ Right url
    '!' : err -> return $ Left err
    str       -> fail $
      "getPicture: internal error, JS function returned " ++ show str
