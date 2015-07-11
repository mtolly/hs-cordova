{-# LANGUAGE TemplateHaskell #-}
module System.Cordova.Device
( cordova, model, platform, uuid, version
) where

import qualified Data.Text as T
import System.Cordova.Binders

jsImport [d|
  cordova :: T.Text
  cordova = jsCode "device.cordova"
  |]
jsImport [d|
  model :: T.Text
  model = jsCode "device.model"
  |]
jsImport [d|
  platform :: T.Text
  platform = jsCode "device.platform"
  |]
jsImport [d|
  uuid :: T.Text
  uuid = jsCode "device.uuid"
  |]
jsImport [d|
  version :: T.Text
  version = jsCode "device.version"
  |]
