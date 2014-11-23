
{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.FileSystem
( applicationDirectory, applicationStorageDirectory, dataDirectory, cacheDirectory, externalApplicationStorageDirectory, externalDataDirectory, externalCacheDirectory, externalRootDirectory, tempDirectory, syncedDataDirectory, documentsDirectory, sharedDirectory
, Storage(..)
, FileError(..), FileErrorCode(..)
, FileSystem
, requestFileSystem
, Entry, Dir, File
, root, filesystem
, fullPath, name
, toURL, toInternalURL
, isFile, isDirectory, classifyEntry, genericEntry
, Metadata(..), getMetadata
, remove, removeRecursively, moveTo, copyTo
, getParent
, resolveLocalFileSystemURL
, GetFlags(..), getFile, getDirectory
, DirReader, createReader, readEntries
, readAllEntries
, FileObject, file
, readAsText, readAsBinaryString, readAsDataURL
) where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import System.Cordova.Internal
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad ((>=>))
import Data.Time.Clock
import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified Data.Default as RDefault
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp



foreign import javascript unsafe
  "cordova.file.applicationDirectory"
  js_applicationDirectory :: JSRef (Maybe String)
applicationDirectory      :: Maybe String
applicationDirectory = unsafePerformIO $ fromRefMaybe js_applicationDirectory

foreign import javascript unsafe
  "cordova.file.applicationStorageDirectory"
  js_applicationStorageDirectory :: JSRef (Maybe String)
applicationStorageDirectory      :: Maybe String
applicationStorageDirectory = unsafePerformIO $ fromRefMaybe js_applicationStorageDirectory

foreign import javascript unsafe
  "cordova.file.dataDirectory"
  js_dataDirectory :: JSRef (Maybe String)
dataDirectory      :: Maybe String
dataDirectory = unsafePerformIO $ fromRefMaybe js_dataDirectory

foreign import javascript unsafe
  "cordova.file.cacheDirectory"
  js_cacheDirectory :: JSRef (Maybe String)
cacheDirectory      :: Maybe String
cacheDirectory = unsafePerformIO $ fromRefMaybe js_cacheDirectory

foreign import javascript unsafe
  "cordova.file.externalApplicationStorageDirectory"
  js_externalApplicationStorageDirectory :: JSRef (Maybe String)
externalApplicationStorageDirectory      :: Maybe String
externalApplicationStorageDirectory = unsafePerformIO $ fromRefMaybe js_externalApplicationStorageDirectory

foreign import javascript unsafe
  "cordova.file.externalDataDirectory"
  js_externalDataDirectory :: JSRef (Maybe String)
externalDataDirectory      :: Maybe String
externalDataDirectory = unsafePerformIO $ fromRefMaybe js_externalDataDirectory

foreign import javascript unsafe
  "cordova.file.externalCacheDirectory"
  js_externalCacheDirectory :: JSRef (Maybe String)
externalCacheDirectory      :: Maybe String
externalCacheDirectory = unsafePerformIO $ fromRefMaybe js_externalCacheDirectory

foreign import javascript unsafe
  "cordova.file.externalRootDirectory"
  js_externalRootDirectory :: JSRef (Maybe String)
externalRootDirectory      :: Maybe String
externalRootDirectory = unsafePerformIO $ fromRefMaybe js_externalRootDirectory

foreign import javascript unsafe
  "cordova.file.tempDirectory"
  js_tempDirectory :: JSRef (Maybe String)
tempDirectory      :: Maybe String
tempDirectory = unsafePerformIO $ fromRefMaybe js_tempDirectory

foreign import javascript unsafe
  "cordova.file.syncedDataDirectory"
  js_syncedDataDirectory :: JSRef (Maybe String)
syncedDataDirectory      :: Maybe String
syncedDataDirectory = unsafePerformIO $ fromRefMaybe js_syncedDataDirectory

foreign import javascript unsafe
  "cordova.file.documentsDirectory"
  js_documentsDirectory :: JSRef (Maybe String)
documentsDirectory      :: Maybe String
documentsDirectory = unsafePerformIO $ fromRefMaybe js_documentsDirectory

foreign import javascript unsafe
  "cordova.file.sharedDirectory"
  js_sharedDirectory :: JSRef (Maybe String)
sharedDirectory      :: Maybe String
sharedDirectory = unsafePerformIO $ fromRefMaybe js_sharedDirectory


data FileErrorCode = NotFoundErr | SecurityErr | AbortErr | NotReadableErr | EncodingErr | NoModificationAllowedErr | InvalidStateErr | SyntaxErr | InvalidModificationErr | QuotaExceededErr | TypeMismatchErr | PathExistsErr deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "FileError.NOT_FOUND_ERR" _FileErrorCode_NotFoundErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.SECURITY_ERR" _FileErrorCode_SecurityErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.ABORT_ERR" _FileErrorCode_AbortErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.NOT_READABLE_ERR" _FileErrorCode_NotReadableErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.ENCODING_ERR" _FileErrorCode_EncodingErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.NO_MODIFICATION_ALLOWED_ERR" _FileErrorCode_NoModificationAllowedErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.INVALID_STATE_ERR" _FileErrorCode_InvalidStateErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.SYNTAX_ERR" _FileErrorCode_SyntaxErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.INVALID_MODIFICATION_ERR" _FileErrorCode_InvalidModificationErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.QUOTA_EXCEEDED_ERR" _FileErrorCode_QuotaExceededErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.TYPE_MISMATCH_ERR" _FileErrorCode_TypeMismatchErr :: RTypes.JSRef FileErrorCode
foreign import javascript unsafe "FileError.PATH_EXISTS_ERR" _FileErrorCode_PathExistsErr :: RTypes.JSRef FileErrorCode
instance RMarshal.ToJSRef FileErrorCode where
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
instance RMarshal.FromJSRef FileErrorCode where
  fromJSRef = RInternal.js_fromEnum

data FileError = FileError { code :: FileErrorCode } deriving (Eq, Ord, Show, Read)
instance RMarshal.ToJSRef FileError where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "code" code
    return obj
instance RMarshal.FromJSRef FileError where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "code" obj
    return $ FileError RApp.<$> _x0

data Storage = Temporary | Persistent deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "window.TEMPORARY" _Storage_Temporary :: RTypes.JSRef Storage
foreign import javascript unsafe "window.PERSISTENT" _Storage_Persistent :: RTypes.JSRef Storage
instance RMarshal.ToJSRef Storage where
  toJSRef Temporary = return _Storage_Temporary
  toJSRef Persistent = return _Storage_Persistent
instance RMarshal.FromJSRef Storage where
  fromJSRef = RInternal.js_fromEnum

data FileSystem_
type FileSystem = JSRef FileSystem_

foreign import javascript interruptible
  "requestFileSystem($1, $2, hs_good($c), hs_error($c));"
  js_requestFileSystem :: JSRef Storage -> Double  -> IO (JSEitherRef FileError FileSystem)
requestFileSystem      :: Storage       -> Integer -> IO (Either      FileError FileSystem)
requestFileSystem stor size = do
  stor' <- toJSRef stor
  js_requestFileSystem stor' (fromIntegral size) >>= fromJSEitherRef

data File
data Dir
data Entry_ a
type Entry a = JSRef (Entry_ a)

foreign import javascript unsafe
  "$1.root" root :: FileSystem -> Entry Dir

foreign import javascript unsafe
  "$1.filesystem" filesystem :: Entry a -> FileSystem


foreign import javascript unsafe
  "$1.fullPath"
  js_fullPath :: Entry a -> JSString
fullPath      :: Entry a -> FilePath
fullPath = fromJSString . js_fullPath

foreign import javascript unsafe
  "$1.name"
  js_name :: Entry a -> JSString
name      :: Entry a -> FilePath
name = fromJSString . js_name



foreign import javascript unsafe
  "$1.toURL()"
  js_toURL :: Entry a -> JSString
toURL      :: Entry a -> String
toURL = fromJSString . js_toURL

foreign import javascript unsafe
  "$1.toInternalURL()"
  js_toInternalURL :: Entry a -> JSString
toInternalURL      :: Entry a -> String
toInternalURL = fromJSString . js_toInternalURL


foreign import javascript unsafe
  "$1.isFile" isFile :: Entry a -> Bool

foreign import javascript unsafe
  "$1.isDirectory" isDirectory :: Entry a -> Bool

classifyEntry :: Entry a -> Either (Entry File) (Entry Dir)
classifyEntry e = if isFile e then Left (castRef e) else Right (castRef e)

genericEntry :: Entry a -> Entry ()
genericEntry = castRef

data Metadata = Metadata { modificationTime :: UTCTime } deriving (Eq, Ord, Show, Read)
instance RMarshal.ToJSRef Metadata where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "modificationTime" modificationTime
    return obj
instance RMarshal.FromJSRef Metadata where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "modificationTime" obj
    return $ Metadata RApp.<$> _x0

foreign import javascript interruptible
  "$1.getMetadata(hs_good($c), hs_error($c));"
  js_getMetadata :: Entry a -> IO (JSEitherRef FileError Metadata)
getMetadata      :: Entry a -> IO (Either      FileError Metadata)
getMetadata = js_getMetadata >=> fromJSEitherRef

-- TODO: setMetadata


foreign import javascript interruptible
  "$1.remove(hs_good($c), hs_error($c));"
  js_remove :: Entry a -> IO (JSEitherRef FileError ())
remove      :: Entry a -> IO (Either      FileError ())
remove = js_remove >=> fromJSEitherRef

foreign import javascript interruptible
  "$1.removeRecursively(hs_good($c), hs_error($c));"
  js_removeRecursively :: Entry Dir -> IO (JSEitherRef FileError ())
removeRecursively      :: Entry Dir -> IO (Either      FileError ())
removeRecursively = js_removeRecursively >=> fromJSEitherRef



foreign import javascript interruptible
  "$3.moveTo($1, $2, hs_good($c), hs_error($c));"
  js_moveTo :: Entry Dir -> JSRef (Maybe String) -> Entry a -> IO (JSEitherRef FileError (Entry a))
moveTo      :: Entry Dir -> Maybe FilePath       -> Entry a -> IO (Either      FileError (Entry a))
moveTo dir new old = do
  new' <- toJSRef new
  js_moveTo dir new' old >>= fromJSEitherRef

foreign import javascript interruptible
  "$3.copyTo($1, $2, hs_good($c), hs_error($c));"
  js_copyTo :: Entry Dir -> JSRef (Maybe String) -> Entry a -> IO (JSEitherRef FileError (Entry a))
copyTo      :: Entry Dir -> Maybe FilePath       -> Entry a -> IO (Either      FileError (Entry a))
copyTo dir new old = do
  new' <- toJSRef new
  js_copyTo dir new' old >>= fromJSEitherRef


foreign import javascript interruptible
  "$1.getParent(hs_good($c), hs_error($c));"
  js_getParent :: Entry a -> IO (JSEitherRef FileError (Entry Dir))
getParent      :: Entry a -> IO (Either      FileError (Entry Dir))
getParent = js_getParent >=> fromJSEitherRef

foreign import javascript interruptible
  "resolveLocalFileSystemURL($1, hs_good($c), hs_error($c));"
  js_resolveLocalFileSystemURL :: JSString -> IO (JSEitherRef FileError (Entry ()))
resolveLocalFileSystemURL      :: String   -> IO (Either      FileError (Entry ()))
resolveLocalFileSystemURL =
  js_resolveLocalFileSystemURL . toJSString >=> fromJSEitherRef

data GetFlags = Exclusive | Create | NoCreate deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "{create: true, exclusive: true}" _GetFlags_Exclusive :: RTypes.JSRef GetFlags
foreign import javascript unsafe "{create: true, exclusive: false}" _GetFlags_Create :: RTypes.JSRef GetFlags
foreign import javascript unsafe "{create: false}" _GetFlags_NoCreate :: RTypes.JSRef GetFlags
instance RMarshal.ToJSRef GetFlags where
  toJSRef Exclusive = return _GetFlags_Exclusive
  toJSRef Create = return _GetFlags_Create
  toJSRef NoCreate = return _GetFlags_NoCreate
instance RMarshal.FromJSRef GetFlags where
  fromJSRef = RInternal.js_fromEnum


foreign import javascript interruptible
  "$3.getFile($1, $2, hs_good($c), hs_error($c));"
  js_getFile :: JSString -> JSRef GetFlags -> Entry Dir -> IO (JSEitherRef FileError (Entry File))
getFile      :: FilePath -> GetFlags       -> Entry Dir -> IO (Either      FileError (Entry File))
getFile f flags dir = do
  flags' <- toJSRef flags
  js_getFile (toJSString f) flags' dir >>= fromJSEitherRef

foreign import javascript interruptible
  "$3.getDirectory($1, $2, hs_good($c), hs_error($c));"
  js_getDirectory :: JSString -> JSRef GetFlags -> Entry Dir -> IO (JSEitherRef FileError (Entry Dir))
getDirectory      :: FilePath -> GetFlags       -> Entry Dir -> IO (Either      FileError (Entry Dir))
getDirectory f flags dir = do
  flags' <- toJSRef flags
  js_getDirectory (toJSString f) flags' dir >>= fromJSEitherRef


data DirReader_
type DirReader = JSRef DirReader_

foreign import javascript unsafe
  "$1.createReader()"
  createReader :: Entry Dir -> IO DirReader

foreign import javascript interruptible
  "$1.readEntries(hs_good($c), hs_error($c));"
  js_readEntries :: DirReader -> IO (JSEitherRef FileError [Entry ()])
readEntries      :: DirReader -> IO (Either      FileError [Entry ()])
readEntries = js_readEntries >=> fromJSEitherRef

readAllEntries :: Entry Dir -> IO (Either FileError [Entry ()])
readAllEntries dir = do
  r <- createReader dir
  let go ents = readEntries r >>= \res -> case res of
        Left e -> return $ Left e
        Right [] -> return $ Right ents
        Right ents' -> go $ ents' ++ ents
  go []

data FileObject_
type FileObject = JSRef FileObject_

foreign import javascript interruptible
  "$1.file(hs_good($c), hs_error($c));"
  js_file :: Entry File -> IO (JSEitherRef FileError FileObject)
file      :: Entry File -> IO (Either      FileError FileObject)
file = js_file >=> fromJSEitherRef


foreign import javascript interruptible
  "hs_readFile('readAsText', $1, $c);"
  js_readAsText :: FileObject -> IO JSString
readAsText      :: FileObject -> IO String
readAsText = fmap fromJSString . js_readAsText

foreign import javascript interruptible
  "hs_readFile('readAsBinaryString', $1, $c);"
  js_readAsBinaryString :: FileObject -> IO JSString
readAsBinaryString      :: FileObject -> IO String
readAsBinaryString = fmap fromJSString . js_readAsBinaryString

foreign import javascript interruptible
  "hs_readFile('readAsDataURL', $1, $c);"
  js_readAsDataURL :: FileObject -> IO JSString
readAsDataURL      :: FileObject -> IO String
readAsDataURL = fmap fromJSString . js_readAsDataURL

