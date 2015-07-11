{-# LANGUAGE TemplateHaskell #-}
module System.Cordova.Device
( cordova, model, platform, uuid, version
) where

import qualified Data.Text as T
import System.Cordova.Binders

jsImport [d|
  cordova :: T.Text
  cordova = "device.cordova"
  |]
jsImport [d|
  model :: T.Text
  model = "device.model"
  |]
jsImport [d|
  platform :: T.Text
  platform = "device.platform"
  |]
jsImport [d|
  uuid :: T.Text
  uuid = "device.uuid"
  |]
jsImport [d|
  version :: T.Text
  version = "device.version"
  |]
