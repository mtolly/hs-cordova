

module System.Cordova.Device
( cordova, model, platform, uuid, version
) where

import qualified GHCJS.Types as RTypes
import qualified System.Cordova.Internal as RInternal
import qualified System.IO.Unsafe as RUnsafe



foreign import javascript unsafe
  "device.cordova"
  js_cordova ::  IO (RTypes.JSRef (String))
cordova ::   (String)
cordova  = RUnsafe.unsafePerformIO $ do
  res <- js_cordova 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "device.model"
  js_model ::  IO (RTypes.JSRef (String))
model ::   (String)
model  = RUnsafe.unsafePerformIO $ do
  res <- js_model 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "device.platform"
  js_platform ::  IO (RTypes.JSRef (String))
platform ::   (String)
platform  = RUnsafe.unsafePerformIO $ do
  res <- js_platform 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "device.uuid"
  js_uuid ::  IO (RTypes.JSRef (String))
uuid ::   (String)
uuid  = RUnsafe.unsafePerformIO $ do
  res <- js_uuid 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "device.version"
  js_version ::  IO (RTypes.JSRef (String))
version ::   (String)
version  = RUnsafe.unsafePerformIO $ do
  res <- js_version 
  RInternal.fromJSRef' res

