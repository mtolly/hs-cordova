

module System.Cordova.Device
( cordova, model, platform, uuid, version
) where

import qualified Data.Text as T
import qualified GHCJS.Types as RTypes
import qualified System.Cordova.Internal as RInternal
import qualified System.IO.Unsafe as RUnsafe



foreign import javascript unsafe
  "device.cordova"
  js_cordova ::  IO (RTypes.JSRef (T.Text))
cordova ::   (T.Text)
cordova  = RUnsafe.unsafePerformIO $ do
  res <- js_cordova 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "device.model"
  js_model ::  IO (RTypes.JSRef (T.Text))
model ::   (T.Text)
model  = RUnsafe.unsafePerformIO $ do
  res <- js_model 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "device.platform"
  js_platform ::  IO (RTypes.JSRef (T.Text))
platform ::   (T.Text)
platform  = RUnsafe.unsafePerformIO $ do
  res <- js_platform 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "device.uuid"
  js_uuid ::  IO (RTypes.JSRef (T.Text))
uuid ::   (T.Text)
uuid  = RUnsafe.unsafePerformIO $ do
  res <- js_uuid 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "device.version"
  js_version ::  IO (RTypes.JSRef (T.Text))
version ::   (T.Text)
version  = RUnsafe.unsafePerformIO $ do
  res <- js_version 
  RInternal.fromJSRef' res

