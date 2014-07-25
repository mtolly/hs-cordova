{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GHCJS.Types
import GHCJS.Foreign

main :: IO ()
main = bindEvents

foreign import javascript safe
  "document.addEventListener($1, $2, false);"
  addEventListener :: JSString -> JSFun (IO ()) -> IO ()

bindEvents :: IO ()
bindEvents = do
  fn <- asyncCallback AlwaysRetain onDeviceReady
  addEventListener "deviceready" fn

onDeviceReady :: IO ()
onDeviceReady = do
  receivedEvent "deviceready"

data Element_
type Element = JSRef Element_

foreign import javascript unsafe
  "document.getElementById($1)"
  getElementById :: JSString -> IO Element

foreign import javascript unsafe
  "$1.querySelector($2)"
  querySelector :: Element -> JSString -> IO Element

foreign import javascript unsafe
  "$1.setAttribute($2, $3);"
  setAttribute :: Element -> JSString -> JSString -> IO ()

receivedEvent :: JSString -> IO ()
receivedEvent evt = do

  parentElement <- getElementById evt
  listeningElement <- querySelector parentElement ".listening"
  receivedElement <- querySelector parentElement ".received"

  setAttribute listeningElement "style" "display: none;"
  setAttribute receivedElement "style" "display: block;"

  putStrLn $ "Received Event: " ++ fromJSString evt
