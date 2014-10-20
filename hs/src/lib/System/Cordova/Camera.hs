{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Camera
( CameraOptions(..)
, DestinationType(..)
, SourceType(..)
, EncodingType(..)
, MediaType(..)
, PopoverOptions(..)
, PopoverArrowDirection(..)
, Direction(..)
, getPicture
, cleanup
) where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import System.Cordova.Internal

import System.Cordova.Camera.DestinationType
import System.Cordova.Camera.SourceType
import System.Cordova.Camera.EncodingType
import System.Cordova.Camera.MediaType
import System.Cordova.Camera.PopoverArrowDirection
import System.Cordova.Camera.Direction

import System.Cordova.Camera.CameraOptions
import System.Cordova.Camera.PopoverOptions

foreign import javascript interruptible
  "navigator.camera.getPicture(hs_good($c), hs_error($c), $1);"
  js_getPicture :: JSRef CameraOptions -> IO (JSEither JSString JSString)

getPicture :: CameraOptions -> IO (Either String String)
getPicture opts = do
  eitherJJ <- toJSRef opts >>= js_getPicture >>= fromJSEither
  return $ case eitherJJ of
    Left err -> Left $ fromJSString err
    Right x -> Right $ fromJSString x

foreign import javascript interruptible
  "navigator.camera.cleanup(hs_good($c), hs_error($c));"
  js_cleanup :: IO (JSEither JSString (JSRef ()))

cleanup :: IO (Maybe String)
cleanup = js_cleanup >>= fromJSEither >>= \x -> return $ case x of
  Left err -> Just $ fromJSString err
  Right _  -> Nothing
