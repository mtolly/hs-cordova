
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
, readAsText, readAsBinaryString, readAsDataURL, readAsBinary
, FileWriter, Blob
, createWriter, seek, textBlob, writeBlob
) where

import GHCJS.Types
import GHCJS.Marshal
import Data.Time.Clock
import qualified Data.ByteString.Char8 as B8
import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp
import qualified System.IO.Unsafe as RUnsafe



foreign import javascript unsafe
  "cordova.file.applicationDirectory"
  js_applicationDirectory ::  IO (RTypes.JSRef (Maybe String))
applicationDirectory ::   (Maybe String)
applicationDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_applicationDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.applicationStorageDirectory"
  js_applicationStorageDirectory ::  IO (RTypes.JSRef (Maybe String))
applicationStorageDirectory ::   (Maybe String)
applicationStorageDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_applicationStorageDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.dataDirectory"
  js_dataDirectory ::  IO (RTypes.JSRef (Maybe String))
dataDirectory ::   (Maybe String)
dataDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_dataDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.cacheDirectory"
  js_cacheDirectory ::  IO (RTypes.JSRef (Maybe String))
cacheDirectory ::   (Maybe String)
cacheDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_cacheDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.externalApplicationStorageDirectory"
  js_externalApplicationStorageDirectory ::  IO (RTypes.JSRef (Maybe String))
externalApplicationStorageDirectory ::   (Maybe String)
externalApplicationStorageDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_externalApplicationStorageDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.externalDataDirectory"
  js_externalDataDirectory ::  IO (RTypes.JSRef (Maybe String))
externalDataDirectory ::   (Maybe String)
externalDataDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_externalDataDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.externalCacheDirectory"
  js_externalCacheDirectory ::  IO (RTypes.JSRef (Maybe String))
externalCacheDirectory ::   (Maybe String)
externalCacheDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_externalCacheDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.externalRootDirectory"
  js_externalRootDirectory ::  IO (RTypes.JSRef (Maybe String))
externalRootDirectory ::   (Maybe String)
externalRootDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_externalRootDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.tempDirectory"
  js_tempDirectory ::  IO (RTypes.JSRef (Maybe String))
tempDirectory ::   (Maybe String)
tempDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_tempDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.syncedDataDirectory"
  js_syncedDataDirectory ::  IO (RTypes.JSRef (Maybe String))
syncedDataDirectory ::   (Maybe String)
syncedDataDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_syncedDataDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.documentsDirectory"
  js_documentsDirectory ::  IO (RTypes.JSRef (Maybe String))
documentsDirectory ::   (Maybe String)
documentsDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_documentsDirectory 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "cordova.file.sharedDirectory"
  js_sharedDirectory ::  IO (RTypes.JSRef (Maybe String))
sharedDirectory ::   (Maybe String)
sharedDirectory  = RUnsafe.unsafePerformIO $ do
  res <- js_sharedDirectory 
  RInternal.fromJSRef' res


data FileErrorCode
  = NotFoundErr
  | SecurityErr
  | AbortErr
  | NotReadableErr
  | EncodingErr
  | NoModificationAllowedErr
  | InvalidStateErr
  | SyntaxErr
  | InvalidModificationErr
  | QuotaExceededErr
  | TypeMismatchErr
  | PathExistsErr
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
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

data FileError = FileError
  { code :: FileErrorCode
  } deriving (Eq, Ord, Show, Read)
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

data Storage
  = Temporary
  | Persistent
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
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
  js_requestFileSystem :: RTypes.JSRef (Storage) -> RTypes.JSRef (Integer) -> IO (RInternal.JSEitherRef FileError FileSystem)
requestFileSystem :: Storage -> Integer -> IO (Either FileError FileSystem)
requestFileSystem arg0 arg1 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  res <- js_requestFileSystem arg0' arg1'
  RInternal.fromJSEitherRef res

data File
data Dir
data Entry_ a
type Entry a = JSRef (Entry_ a)

foreign import javascript unsafe
  "$1.root"
  js_root :: RTypes.JSRef (FileSystem) -> IO (RTypes.JSRef (Entry Dir))
root :: FileSystem ->  (Entry Dir)
root arg0 = RUnsafe.unsafePerformIO $ do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_root arg0'
  RInternal.fromJSRef' res
foreign import javascript unsafe
  "$1.filesystem"
  js_filesystem :: RTypes.JSRef (Entry a) -> IO (RTypes.JSRef (FileSystem))
filesystem :: Entry a ->  (FileSystem)
filesystem arg0 = RUnsafe.unsafePerformIO $ do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_filesystem arg0'
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "$1.fullPath"
  js_fullPath :: RTypes.JSRef (Entry a) -> IO (RTypes.JSRef (FilePath))
fullPath :: Entry a ->  (FilePath)
fullPath arg0 = RUnsafe.unsafePerformIO $ do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_fullPath arg0'
  RInternal.fromJSRef' res
foreign import javascript unsafe
  "$1.name"
  js_name :: RTypes.JSRef (Entry a) -> IO (RTypes.JSRef (FilePath))
name :: Entry a ->  (FilePath)
name arg0 = RUnsafe.unsafePerformIO $ do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_name arg0'
  RInternal.fromJSRef' res
foreign import javascript unsafe
  "$1.toURL()"
  js_toURL :: RTypes.JSRef (Entry a) -> IO (RTypes.JSRef (String))
toURL :: Entry a ->  (String)
toURL arg0 = RUnsafe.unsafePerformIO $ do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_toURL arg0'
  RInternal.fromJSRef' res
foreign import javascript unsafe
  "$1.toInternalURL()"
  js_toInternalURL :: RTypes.JSRef (Entry a) -> IO (RTypes.JSRef (String))
toInternalURL :: Entry a ->  (String)
toInternalURL arg0 = RUnsafe.unsafePerformIO $ do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_toInternalURL arg0'
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "$1.isFile"
  js_isFile :: RTypes.JSRef (Entry a) -> IO (RTypes.JSRef (Bool))
isFile :: Entry a ->  (Bool)
isFile arg0 = RUnsafe.unsafePerformIO $ do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_isFile arg0'
  RInternal.fromJSRef' res
foreign import javascript unsafe
  "$1.isDirectory"
  js_isDirectory :: RTypes.JSRef (Entry a) -> IO (RTypes.JSRef (Bool))
isDirectory :: Entry a ->  (Bool)
isDirectory arg0 = RUnsafe.unsafePerformIO $ do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_isDirectory arg0'
  RInternal.fromJSRef' res

classifyEntry :: Entry a -> Either (Entry File) (Entry Dir)
classifyEntry e
  | isFile      e = Left  $ castRef e
  | isDirectory e = Right $ castRef e
  | otherwise     = error "classifyEntry: Entry is neither File nor Dir"

genericEntry :: Entry a -> Entry ()
genericEntry = castRef

data Metadata = Metadata
  { modificationTime :: UTCTime
  } deriving (Eq, Ord, Show, Read)
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
  js_getMetadata :: RTypes.JSRef (Entry a) -> IO (RInternal.JSEitherRef FileError Metadata)
getMetadata :: Entry a -> IO (Either FileError Metadata)
getMetadata arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_getMetadata arg0'
  RInternal.fromJSEitherRef res

-- TODO: setMetadata

foreign import javascript interruptible
  "$1.remove(hs_good($c), hs_error($c));"
  js_remove :: RTypes.JSRef (Entry a) -> IO (RInternal.JSEitherRef FileError ())
remove :: Entry a -> IO (Either FileError ())
remove arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_remove arg0'
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "$1.removeRecursively(hs_good($c), hs_error($c));"
  js_removeRecursively :: RTypes.JSRef (Entry Dir) -> IO (RInternal.JSEitherRef FileError ())
removeRecursively :: Entry Dir -> IO (Either FileError ())
removeRecursively arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_removeRecursively arg0'
  RInternal.fromJSEitherRef res


foreign import javascript interruptible
  "$1.moveTo(hs_good($c), hs_error($c));"
  js_moveTo :: RTypes.JSRef (Entry Dir) -> RTypes.JSRef (Maybe FilePath) -> RTypes.JSRef (Entry a) -> IO (RInternal.JSEitherRef FileError (Entry a))
moveTo :: Entry Dir -> Maybe FilePath -> Entry a -> IO (Either FileError (Entry a))
moveTo arg0 arg1 arg2 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  arg2' <- RMarshal.toJSRef arg2
  res <- js_moveTo arg0' arg1' arg2'
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "$1.copyTo(hs_good($c), hs_error($c));"
  js_copyTo :: RTypes.JSRef (Entry Dir) -> RTypes.JSRef (Maybe FilePath) -> RTypes.JSRef (Entry a) -> IO (RInternal.JSEitherRef FileError (Entry a))
copyTo :: Entry Dir -> Maybe FilePath -> Entry a -> IO (Either FileError (Entry a))
copyTo arg0 arg1 arg2 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  arg2' <- RMarshal.toJSRef arg2
  res <- js_copyTo arg0' arg1' arg2'
  RInternal.fromJSEitherRef res


foreign import javascript interruptible
  "$1.getParent(hs_good($c), hs_error($c));"
  js_getParent :: RTypes.JSRef (Entry a) -> IO (RInternal.JSEitherRef FileError (Entry Dir))
getParent :: Entry a -> IO (Either FileError (Entry Dir))
getParent arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_getParent arg0'
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "resolveLocalFileSystemURL($1, hs_good($c), hs_error($c));"
  js_resolveLocalFileSystemURL :: RTypes.JSRef (String) -> IO (RInternal.JSEitherRef FileError (Entry ()))
resolveLocalFileSystemURL :: String -> IO (Either FileError (Entry ()))
resolveLocalFileSystemURL arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_resolveLocalFileSystemURL arg0'
  RInternal.fromJSEitherRef res

data GetFlags
  = Exclusive
  | Create
  | NoCreate
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "{create: true, exclusive: true}" _GetFlags_Exclusive :: RTypes.JSRef GetFlags
foreign import javascript unsafe "{create: true, exclusive: false}" _GetFlags_Create :: RTypes.JSRef GetFlags
foreign import javascript unsafe "{create: false}" _GetFlags_NoCreate :: RTypes.JSRef GetFlags
instance RMarshal.ToJSRef GetFlags where
  toJSRef Exclusive = return _GetFlags_Exclusive
  toJSRef Create = return _GetFlags_Create
  toJSRef NoCreate = return _GetFlags_NoCreate

foreign import javascript interruptible
  "$3.getFile($1, $2, hs_good($c), hs_error($c));"
  js_getFile :: RTypes.JSRef (FilePath) -> RTypes.JSRef (GetFlags) -> RTypes.JSRef (Entry Dir) -> IO (RInternal.JSEitherRef FileError (Entry File))
getFile :: FilePath -> GetFlags -> Entry Dir -> IO (Either FileError (Entry File))
getFile arg0 arg1 arg2 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  arg2' <- RMarshal.toJSRef arg2
  res <- js_getFile arg0' arg1' arg2'
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "$3.getDirectory($1, $2, hs_good($c), hs_error($c));"
  js_getDirectory :: RTypes.JSRef (FilePath) -> RTypes.JSRef (GetFlags) -> RTypes.JSRef (Entry Dir) -> IO (RInternal.JSEitherRef FileError (Entry Dir))
getDirectory :: FilePath -> GetFlags -> Entry Dir -> IO (Either FileError (Entry Dir))
getDirectory arg0 arg1 arg2 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  arg2' <- RMarshal.toJSRef arg2
  res <- js_getDirectory arg0' arg1' arg2'
  RInternal.fromJSEitherRef res

data DirReader_
type DirReader = JSRef DirReader_

foreign import javascript unsafe
  "$1.createReader()"
  js_createReader :: RTypes.JSRef (Entry Dir) -> IO (RTypes.JSRef (DirReader))
createReader :: Entry Dir -> IO (DirReader)
createReader arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_createReader arg0'
  RInternal.fromJSRef' res

foreign import javascript interruptible
  "$1.readEntries(hs_good($c), hs_error($c));"
  js_readEntries :: RTypes.JSRef (DirReader) -> IO (RInternal.JSEitherRef FileError [Entry ()])
readEntries :: DirReader -> IO (Either FileError [Entry ()])
readEntries arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_readEntries arg0'
  RInternal.fromJSEitherRef res

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
  js_file :: RTypes.JSRef (Entry File) -> IO (RInternal.JSEitherRef FileError FileObject)
file :: Entry File -> IO (Either FileError FileObject)
file arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_file arg0'
  RInternal.fromJSEitherRef res


foreign import javascript interruptible
  "hs_readFile('readAsText', $1, $c);"
  js_readAsText :: RTypes.JSRef (FileObject) -> IO (RInternal.JSEitherRef FileError String)
readAsText :: FileObject -> IO (Either FileError String)
readAsText arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_readAsText arg0'
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "hs_readFile('readAsBinaryString', $1, $c);"
  js_readAsBinaryString :: RTypes.JSRef (FileObject) -> IO (RInternal.JSEitherRef FileError String)
readAsBinaryString :: FileObject -> IO (Either FileError String)
readAsBinaryString arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_readAsBinaryString arg0'
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "hs_readFile('readAsDataURL', $1, $c);"
  js_readAsDataURL :: RTypes.JSRef (FileObject) -> IO (RInternal.JSEitherRef FileError String)
readAsDataURL :: FileObject -> IO (Either FileError String)
readAsDataURL arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_readAsDataURL arg0'
  RInternal.fromJSEitherRef res


-- TODO: better implementation
readAsBinary :: FileObject -> IO (Either FileError B8.ByteString)
readAsBinary = fmap (fmap B8.pack) . readAsBinaryString

data FileWriter_
type FileWriter = JSRef FileWriter_

foreign import javascript interruptible
  "$1.createWriter(hs_good($c), hs_error($c));"
  js_createWriter :: RTypes.JSRef (Entry File) -> IO (RInternal.JSEitherRef FileError FileWriter)
createWriter :: Entry File -> IO (Either FileError FileWriter)
createWriter arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_createWriter arg0'
  RInternal.fromJSEitherRef res

foreign import javascript unsafe
  "$1.seek($2);"
  js_seek :: RTypes.JSRef (FileWriter) -> RTypes.JSRef (Int) -> IO (RTypes.JSRef (()))
seek :: FileWriter -> Int -> IO (())
seek arg0 arg1 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  res <- js_seek arg0' arg1'
  RInternal.fromJSRef' res

data Blob_
type Blob = JSRef Blob_

foreign import javascript unsafe
  "new Blob([$1], {type: \"text/plain\"})"
  js_textBlob :: RTypes.JSRef (String) -> IO (RTypes.JSRef (Blob))
textBlob :: String ->  (Blob)
textBlob arg0 = RUnsafe.unsafePerformIO $ do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_textBlob arg0'
  RInternal.fromJSRef' res

foreign import javascript interruptible
  "hs_writeBlob($1, $2, $c);"
  js_writeBlob :: RTypes.JSRef (Blob) -> RTypes.JSRef (FileWriter) -> IO (RInternal.JSEitherRef FileError ())
writeBlob :: Blob -> FileWriter -> IO (Either FileError ())
writeBlob arg0 arg1 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  res <- js_writeBlob arg0' arg1'
  RInternal.fromJSEitherRef res
