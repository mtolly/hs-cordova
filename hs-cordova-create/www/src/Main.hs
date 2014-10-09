{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GHCJS.Types
import GHCJS.Foreign

main :: IO ()
main = onDeviceReady

onDeviceReady :: IO ()
onDeviceReady = receivedEvent "deviceready"

data Element_
type Element = JSRef Element_

foreign import javascript unsafe
  "document.getElementById($1)"
  getElementById :: JSString -> IO Element

foreign import javascript unsafe
  "$2.querySelector($1)"
  querySelector :: JSString -> Element -> IO Element

foreign import javascript unsafe
  "$3.setAttribute($1, $2);"
  setAttribute :: JSString -> JSString -> Element -> IO ()

receivedEvent :: JSString -> IO ()
receivedEvent evt = do

  parentElement <- getElementById evt
  listeningElement <- querySelector ".listening" parentElement
  receivedElement <- querySelector ".received" parentElement

  setAttribute "style" "display: none;" listeningElement
  setAttribute "style" "display: block;" receivedElement

  putStrLn $ "Received Event: " ++ fromJSString evt
