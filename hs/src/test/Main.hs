{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.Cordova.Base
import qualified System.Cordova.StatusBar as Bar
import qualified System.Cordova.Device as Dev
import qualified System.Cordova.Geolocation as Geo
import Data.Default
import GHCJS.Types
import GHCJS.Foreign
import Data.IORef

main :: IO ()
main = do
  waitDeviceReady

  Bar.overlaysWebView False
  Bar.styleBlackOpaque
  Bar.backgroundColorByName "black"

  let new :: String -> String -> IO Element
      new tag s = do
        elt <- createElement $ toJSString tag
        setHTML (toJSString s) elt
        appendChild elt body
        return elt

  _ <- new "h1" $ "Device"
  _ <- new "div" $ "cordova: "  ++ show Dev.cordova
  _ <- new "div" $ "model: "    ++ show Dev.model
  _ <- new "div" $ "platform: " ++ show Dev.platform
  _ <- new "div" $ "uuid: "     ++ show Dev.uuid
  _ <- new "div" $ "version: "  ++ show Dev.version

  _ <- new "h1" $ "Geolocation"
  do
    posn <- new "div" $ "Position here"
    btn <- new "button" "Update position"
    onclick btn $ do
      res <- Geo.getCurrentPosition def
      setHTML (toJSString $ show res) posn
    toggle <- new "button" "Start listening"
    stopper <- newIORef Nothing
    onclick toggle $ readIORef stopper >>= \case
      Nothing -> do
        newStopper <- Geo.watchPosition def $ \res ->
          setHTML (toJSString $ show res) posn
        writeIORef stopper $ Just newStopper
        setHTML "Stop listening" toggle
      Just oldStopper -> do
        oldStopper
        setHTML "Start listening" toggle

  return ()

data Element_
type Element = JSRef Element_

foreign import javascript unsafe
  "document.getElementsByTagName('body')[0]"
  body :: Element

foreign import javascript unsafe
  "$2.appendChild($1);"
  appendChild :: Element -> Element -> IO ()

foreign import javascript unsafe
  "document.createElement($1)"
  createElement :: JSString -> IO Element

foreign import javascript unsafe
  "$2.innerHTML = $1;"
  setHTML :: JSString -> Element -> IO ()

foreign import javascript unsafe
  "$3.setAttribute($1, $2);"
  setAttribute :: JSString -> JSString -> Element -> IO ()

foreign import javascript unsafe
  "$1.onclick = $2;"
  js_onclick :: Element -> JSFun (IO ()) -> IO ()

onclick :: Element -> IO () -> IO ()
onclick elt fn = do
  cb <- asyncCallback (DomRetain $ castRef elt) fn
  js_onclick elt cb
