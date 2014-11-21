
module System.Cordova.FileSystem.FileErrorCode where
import GHCJS.Types
import GHCJS.Marshal
import System.Cordova.Internal
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
