
module System.Cordova.FileSystem
( applicationDirectory, applicationStorageDirectory, dataDirectory, cacheDirectory, externalApplicationStorageDirectory, externalDataDirectory, externalCacheDirectory, externalRootDirectory, tempDirectory, syncedDataDirectory, documentsDirectory, sharedDirectory
, Storage(..)
, FileErrorCode(..)
) where

import GHCJS.Types (JSRef)
import System.Cordova.Internal
import System.IO.Unsafe (unsafePerformIO)

import System.Cordova.FileSystem.Storage
import System.Cordova.FileSystem.FileErrorCode

foreign import javascript unsafe "cordova.file.applicationDirectory" js_applicationDirectory :: JSRef (Maybe String)
applicationDirectory :: Maybe String
applicationDirectory = unsafePerformIO $ fromRefMaybe js_applicationDirectory

foreign import javascript unsafe "cordova.file.applicationStorageDirectory" js_applicationStorageDirectory :: JSRef (Maybe String)
applicationStorageDirectory :: Maybe String
applicationStorageDirectory = unsafePerformIO $ fromRefMaybe js_applicationStorageDirectory

foreign import javascript unsafe "cordova.file.dataDirectory" js_dataDirectory :: JSRef (Maybe String)
dataDirectory :: Maybe String
dataDirectory = unsafePerformIO $ fromRefMaybe js_dataDirectory

foreign import javascript unsafe "cordova.file.cacheDirectory" js_cacheDirectory :: JSRef (Maybe String)
cacheDirectory :: Maybe String
cacheDirectory = unsafePerformIO $ fromRefMaybe js_cacheDirectory

foreign import javascript unsafe "cordova.file.externalApplicationStorageDirectory" js_externalApplicationStorageDirectory :: JSRef (Maybe String)
externalApplicationStorageDirectory :: Maybe String
externalApplicationStorageDirectory = unsafePerformIO $ fromRefMaybe js_externalApplicationStorageDirectory

foreign import javascript unsafe "cordova.file.externalDataDirectory" js_externalDataDirectory :: JSRef (Maybe String)
externalDataDirectory :: Maybe String
externalDataDirectory = unsafePerformIO $ fromRefMaybe js_externalDataDirectory

foreign import javascript unsafe "cordova.file.externalCacheDirectory" js_externalCacheDirectory :: JSRef (Maybe String)
externalCacheDirectory :: Maybe String
externalCacheDirectory = unsafePerformIO $ fromRefMaybe js_externalCacheDirectory

foreign import javascript unsafe "cordova.file.externalRootDirectory" js_externalRootDirectory :: JSRef (Maybe String)
externalRootDirectory :: Maybe String
externalRootDirectory = unsafePerformIO $ fromRefMaybe js_externalRootDirectory

foreign import javascript unsafe "cordova.file.tempDirectory" js_tempDirectory :: JSRef (Maybe String)
tempDirectory :: Maybe String
tempDirectory = unsafePerformIO $ fromRefMaybe js_tempDirectory

foreign import javascript unsafe "cordova.file.syncedDataDirectory" js_syncedDataDirectory :: JSRef (Maybe String)
syncedDataDirectory :: Maybe String
syncedDataDirectory = unsafePerformIO $ fromRefMaybe js_syncedDataDirectory

foreign import javascript unsafe "cordova.file.documentsDirectory" js_documentsDirectory :: JSRef (Maybe String)
documentsDirectory :: Maybe String
documentsDirectory = unsafePerformIO $ fromRefMaybe js_documentsDirectory

foreign import javascript unsafe "cordova.file.sharedDirectory" js_sharedDirectory :: JSRef (Maybe String)
sharedDirectory :: Maybe String
sharedDirectory = unsafePerformIO $ fromRefMaybe js_sharedDirectory

