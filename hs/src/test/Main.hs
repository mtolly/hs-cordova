{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Cordova.Base
import qualified System.Cordova.StatusBar as Bar
import qualified System.Cordova.Device as Dev
import qualified System.Cordova.Geolocation as Geo
import qualified System.Cordova.DeviceOrientation as DO
import qualified System.Cordova.DeviceMotion as DM
import Data.Default
import GHCJS.Types
import GHCJS.Foreign
import Control.Concurrent.MVar
import Data.Functor (void)

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

  let newToggle :: IO (IO ()) -> IO ()
      newToggle starter = do
        stopper <- newMVar Nothing
        btn <- new "button" "Start listening"
        onclick btn $ takeMVar stopper >>= \case
          Nothing -> do
            newStopper <- starter
            setHTML "Stop listening" btn
            putMVar stopper $ Just newStopper
          Just oldStopper -> do
            oldStopper
            setHTML "Start listening" btn
            putMVar stopper Nothing

  _ <- new "h1" "Device"
  _ <- new "div" $ "cordova: "  ++ show Dev.cordova
  _ <- new "div" $ "model: "    ++ show Dev.model
  _ <- new "div" $ "platform: " ++ show Dev.platform
  _ <- new "div" $ "uuid: "     ++ show Dev.uuid
  _ <- new "div" $ "version: "  ++ show Dev.version

  _ <- new "h1" "Geolocation"
  void $ do
    result <- new "div" $ "Position here"
    btn <- new "button" "Update position"
    onclick btn $ do
      res <- Geo.getCurrentPosition def
      setHTML (toJSString $ show res) result
    newToggle $ Geo.watchPosition def $ \res ->
      setHTML (toJSString $ show res) result

  _ <- new "h1" "Device Orientation"
  void $ do
    result <- new "div" $ "Direction here"
    btn <- new "button" "Update direction"
    onclick btn $ do
      res <- DO.getCurrentHeading
      setHTML (toJSString $ show res) result
    newToggle $ DO.watchHeading def $ \res ->
      setHTML (toJSString $ show res) result

  _ <- new "h1" "Device Motion"
  void $ do
    result <- new "div" $ "Motion here"
    btn <- new "button" "Update motion"
    onclick btn $ do
      res <- DM.getCurrentAcceleration
      setHTML (toJSString $ show res) result
    newToggle $ DM.watchAcceleration def $ \res ->
      setHTML (toJSString $ show res) result

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
  "$1.onclick = $2;"
  js_onclick :: Element -> JSFun (IO ()) -> IO ()

onclick :: Element -> IO () -> IO ()
onclick elt fn = do
  cb <- asyncCallback (DomRetain $ castRef elt) fn
  js_onclick elt cb
