{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.FileSystem where

import GHCJS.Types
import GHCJS.Foreign

import System.Cordova.Internal

data Storage_
type Storage = JSRef Storage_

foreign import javascript unsafe
  "window.TEMPORARY"
  temporary :: Storage

foreign import javascript unsafe
  "window.PERSISTENT"
  persistent :: Storage

foreign import javascript interruptible
  "window.requestFileSystem($1, $2, function(fs) { $c([0, fs]); }, function(err) { $c([1, err]); });"
  js_requestFileSystem :: Storage -> Int -> IO (JSEither FileError FileSystem)

data FileSystem_
type FileSystem = JSRef FileSystem_

data FileError_
type FileError = JSRef FileError_

requestFileSystem :: Storage -> Int -> IO (Either FileError FileSystem)
requestFileSystem store size = js_requestFileSystem store size >>= fromJSEither

data DirectoryEntry_
type DirectoryEntry = JSRef DirectoryEntry_

foreign import javascript unsafe
  "$1.root"
  root :: FileSystem -> DirectoryEntry

foreign import javascript interruptible
  "$1.getFile($2, $3, function(fs) { $c([0, fs]); }, function(err) { $c([1, err]); });"
  js_getFile :: DirectoryEntry -> JSString -> JSObject a -> IO (JSEither FileError FileEntry)

data GetFileOpts
  = CreateExclusive
  | CreateNonExclusive
  | NoCreate
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data FileEntry_
type FileEntry = JSRef FileEntry_

getFile :: DirectoryEntry -> JSString -> GetFileOpts -> IO (Either FileError FileEntry)
getFile dir fp opts = do
  optObj <- newObj
  case opts of
    CreateExclusive -> do
      setProp "create" jsTrue optObj
      setProp "exclusive" jsTrue optObj
    CreateNonExclusive -> do
      setProp "create" jsTrue optObj
      setProp "exclusive" jsFalse optObj
    NoCreate -> do
      setProp "create" jsFalse optObj
  js_getFile dir fp optObj >>= fromJSEither

foreign import javascript unsafe
  "$1.createReader()"
  createReader :: DirectoryEntry -> IO DirectoryReader

data DirectoryReader_
type DirectoryReader = JSRef DirectoryReader_

foreign import javascript interruptible
  "$1.readEntries(function(fs) { $c([0, fs]); }, function(err) { $c([1, err]); });"
  js_readEntries :: DirectoryReader -> IO (JSEither FileError (JSArray FileEntry_))

readEntries :: DirectoryReader -> IO (Either FileError (JSArray FileEntry_))
readEntries dr = js_readEntries dr >>= fromJSEither
