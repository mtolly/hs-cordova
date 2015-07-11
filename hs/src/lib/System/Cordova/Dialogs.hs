{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Cordova.Dialogs
( alert
, confirm
, PromptResult(..)
, prompt
, beep
) where

import qualified Data.Text as T
import System.Cordova.Binders
import GHCJS.Marshal

jsImport [d|
  alert :: T.Text -> Maybe T.Text -> Maybe T.Text -> IO Int
  alert = "navigator.notification.alert($1, $c, $2, $3);"
  |]

jsImport [d|
  confirm :: T.Text -> Maybe T.Text -> Maybe [T.Text] -> IO Int
  confirm = "navigator.notification.confirm($1, $c, $2, $3);"
  |]

jsRecord [d|
  data PromptResult = PromptResult
    { buttonIndex :: Int
    , input1      :: T.Text
    } deriving (Eq, Ord, Show, Read, ToJSRef, FromJSRef)
  |]

jsImport [d|
  prompt :: T.Text -> Maybe T.Text -> Maybe [T.Text] -> Maybe T.Text -> IO PromptResult
  prompt = "navigator.notification.prompt($1, $c, $2, $3, $4);"
  |]

jsImport [d|
  beep :: Int -> IO ()
  beep = "navigator.notification.beep($1);"
  |]
