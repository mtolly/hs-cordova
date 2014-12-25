{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Cordova.Base
import qualified System.Cordova.StatusBar as Bar
import qualified System.Cordova.Device as Dev
import qualified System.Cordova.Geolocation as Geo
import qualified System.Cordova.DeviceOrientation as DO
import qualified System.Cordova.DeviceMotion as DM
import qualified System.Cordova.Vibration as Vib
import Data.Default
import GHCJS.Types
import GHCJS.Foreign
import Control.Concurrent.MVar
import Data.Functor (void)
import Control.Applicative ((<$>))

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
      new_ :: String -> String -> IO ()
      new_ tag s = void $ new tag s

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

  new_ "h1" "Device"
  new_ "div" $ "cordova: "  ++ show Dev.cordova
  new_ "div" $ "model: "    ++ show Dev.model
  new_ "div" $ "platform: " ++ show Dev.platform
  new_ "div" $ "uuid: "     ++ show Dev.uuid
  new_ "div" $ "version: "  ++ show Dev.version

  new_ "h1" "Geolocation"
  void $ do
    result <- new "div" $ "Position here"
    btn <- new "button" "Update position"
    onclick btn $ do
      res <- Geo.getCurrentPosition def
      setHTML (toJSString $ show res) result
    newToggle $ Geo.watchPosition def $ \res ->
      setHTML (toJSString $ show res) result

  new_ "h1" "Device Orientation"
  void $ do
    result <- new "div" $ "Direction here"
    btn <- new "button" "Update direction"
    onclick btn $ do
      res <- DO.getCurrentHeading
      setHTML (toJSString $ show res) result
    newToggle $ DO.watchHeading def $ \res ->
      setHTML (toJSString $ show res) result

  new_ "h1" "Device Motion"
  void $ do
    result <- new "div" $ "Motion here"
    btn <- new "button" "Update motion"
    onclick btn $ do
      res <- DM.getCurrentAcceleration
      setHTML (toJSString $ show res) result
    newToggle $ DM.watchAcceleration def $ \res ->
      setHTML (toJSString $ show res) result

  new_ "h1" "Vibration"
  void $ do
    btn <- new "button" "Vibrate"
    onclick btn Vib.vibrate
  new_ "br" ""
  void $ do
    btn <- new "button" "Vibrate for"
    txt <- new "input" ""
    setAttribute "type" "text" txt
    onclick btn $ do
      int <- read . fromJSString <$> getValue txt
      Vib.vibrateFor int
  new_ "br" ""
  void $ do
    btn <- new "button" "Vibrate pattern"
    txt <- new "input" ""
    setAttribute "type" "text" txt
    onclick btn $ do
      int <- read . fromJSString <$> getValue txt
      Vib.vibratePattern int
  new_ "br" ""
  void $ do
    btn <- new "button" "Vibrate cancel"
    onclick btn Vib.vibrateCancel

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
  "$1.value"
  getValue :: Element -> IO JSString

foreign import javascript unsafe
  "$1.onclick = $2;"
  js_onclick :: Element -> JSFun (IO ()) -> IO ()

onclick :: Element -> IO () -> IO ()
onclick elt fn = do
  cb <- asyncCallback (DomRetain $ castRef elt) fn
  js_onclick elt cb
