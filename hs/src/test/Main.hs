{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Cordova.Base
import System.Cordova.StatusBar
import System.Cordova.Geolocation
import Data.Default
import GHCJS.Types
import GHCJS.Foreign

main :: IO ()
main = do
  waitDeviceReady

  overlaysWebView False
  styleBlackOpaque
  backgroundColorByName "black"

  setAttribute "style" "font-size: 20px;" body

  debug <- createElement "div"
  appendChild debug body
  _ <- watchPosition def $ \x -> case x of
    Left err -> setHTML (toJSString $ show err) debug
    Right res -> setHTML (toJSString $ show res) debug

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
