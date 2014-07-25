{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GHCJS.Types
import GHCJS.Foreign
import Control.Concurrent (threadDelay)

foreign import javascript unsafe
  "window.isDeviceReady"
  isDeviceReady :: IO Bool

main :: IO ()
main = do
  waitDeviceReady
  onDeviceReady

waitDeviceReady :: IO ()
waitDeviceReady = do
  b <- isDeviceReady
  if b then return () else threadDelay 1000 >> waitDeviceReady

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
