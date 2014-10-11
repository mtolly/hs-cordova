{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Base where

foreign import javascript interruptible
  "hs_onDeviceReady($c);"
  waitDeviceReady :: IO ()
