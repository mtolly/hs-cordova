
module System.Cordova.Device
( cordova, model, platform, uuid, version
) where

import GHCJS.Types (JSString)
import GHCJS.Foreign (fromJSString)



foreign import javascript unsafe
  "device.cordova"
  js_cordova :: JSString
cordova :: String
cordova = fromJSString js_cordova



foreign import javascript unsafe
  "device.model"
  js_model :: JSString
model :: String
model = fromJSString js_model



foreign import javascript unsafe
  "device.platform"
  js_platform :: JSString
platform :: String
platform = fromJSString js_platform



foreign import javascript unsafe
  "device.uuid"
  js_uuid :: JSString
uuid :: String
uuid = fromJSString js_uuid



foreign import javascript unsafe
  "device.version"
  js_version :: JSString
version :: String
version = fromJSString js_version


