
module System.Cordova.FileSystem
( applicationDirectory, applicationStorageDirectory, dataDirectory, cacheDirectory, externalApplicationStorageDirectory, externalDataDirectory, externalCacheDirectory, externalRootDirectory, tempDirectory, syncedDataDirectory, documentsDirectory, sharedDirectory
, Storage(..)
, FileErrorCode(..)
) where

import GHCJS.Types (JSRef)
import GHCJS.Marshal
import System.Cordova.Internal
import System.IO.Unsafe (unsafePerformIO)



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


data Storage = Temporary | Persistent deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "window.TEMPORARY" _Storage_Temporary :: JSRef Storage
foreign import javascript unsafe "window.PERSISTENT" _Storage_Persistent :: JSRef Storage
instance ToJSRef Storage where
  toJSRef Temporary = return _Storage_Temporary
  toJSRef Persistent = return _Storage_Persistent
instance FromJSRef Storage where
  fromJSRef = js_fromEnum

data FileErrorCode = NotFoundErr | SecurityErr | AbortErr | NotReadableErr | EncodingErr | NoModificationAllowedErr | InvalidStateErr | SyntaxErr | InvalidModificationErr | QuotaExceededErr | TypeMismatchErr | PathExistsErr deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "FileError.NOT_FOUND_ERR" _FileErrorCode_NotFoundErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.SECURITY_ERR" _FileErrorCode_SecurityErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.ABORT_ERR" _FileErrorCode_AbortErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.NOT_READABLE_ERR" _FileErrorCode_NotReadableErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.ENCODING_ERR" _FileErrorCode_EncodingErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.NO_MODIFICATION_ALLOWED_ERR" _FileErrorCode_NoModificationAllowedErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.INVALID_STATE_ERR" _FileErrorCode_InvalidStateErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.SYNTAX_ERR" _FileErrorCode_SyntaxErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.INVALID_MODIFICATION_ERR" _FileErrorCode_InvalidModificationErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.QUOTA_EXCEEDED_ERR" _FileErrorCode_QuotaExceededErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.TYPE_MISMATCH_ERR" _FileErrorCode_TypeMismatchErr :: JSRef FileErrorCode
foreign import javascript unsafe "FileError.PATH_EXISTS_ERR" _FileErrorCode_PathExistsErr :: JSRef FileErrorCode
instance ToJSRef FileErrorCode where
  toJSRef NotFoundErr = return _FileErrorCode_NotFoundErr
  toJSRef SecurityErr = return _FileErrorCode_SecurityErr
  toJSRef AbortErr = return _FileErrorCode_AbortErr
  toJSRef NotReadableErr = return _FileErrorCode_NotReadableErr
  toJSRef EncodingErr = return _FileErrorCode_EncodingErr
  toJSRef NoModificationAllowedErr = return _FileErrorCode_NoModificationAllowedErr
  toJSRef InvalidStateErr = return _FileErrorCode_InvalidStateErr
  toJSRef SyntaxErr = return _FileErrorCode_SyntaxErr
  toJSRef InvalidModificationErr = return _FileErrorCode_InvalidModificationErr
  toJSRef QuotaExceededErr = return _FileErrorCode_QuotaExceededErr
  toJSRef TypeMismatchErr = return _FileErrorCode_TypeMismatchErr
  toJSRef PathExistsErr = return _FileErrorCode_PathExistsErr
instance FromJSRef FileErrorCode where
  fromJSRef = js_fromEnum
