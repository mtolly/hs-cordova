
{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Globalization
( GlobalizationError(..)
, GlobalizationErrorCode(..)
, getPreferredLanguage
) where

import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified Data.Default as RDefault
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp


data GlobalizationError = GlobalizationError
  { code :: GlobalizationErrorCode
  , message :: String
  } deriving (Eq, Ord, Show, Read)
instance  RMarshal.ToJSRef (GlobalizationError) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "code" code
    _set "message" message
    return obj
instance  RMarshal.FromJSRef (GlobalizationError) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "code" obj
    _x1 <- RInternal.fromProp "message" obj
    return $ GlobalizationError RApp.<$> _x0 RApp.<*> _x1

data GlobalizationErrorCode
  = UnknownError
  | FormattingError
  | ParsingError
  | PatternError
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "GlobalizationError.UNKNOWN_ERROR" _GlobalizationErrorCode_UnknownError :: RTypes.JSRef GlobalizationErrorCode
foreign import javascript unsafe "GlobalizationError.FORMATTING_ERROR" _GlobalizationErrorCode_FormattingError :: RTypes.JSRef GlobalizationErrorCode
foreign import javascript unsafe "GlobalizationError.PARSING_ERROR" _GlobalizationErrorCode_ParsingError :: RTypes.JSRef GlobalizationErrorCode
foreign import javascript unsafe "GlobalizationError.PATTERN_ERROR" _GlobalizationErrorCode_PatternError :: RTypes.JSRef GlobalizationErrorCode
instance RMarshal.ToJSRef GlobalizationErrorCode where
  toJSRef UnknownError = return _GlobalizationErrorCode_UnknownError
  toJSRef FormattingError = return _GlobalizationErrorCode_FormattingError
  toJSRef ParsingError = return _GlobalizationErrorCode_ParsingError
  toJSRef PatternError = return _GlobalizationErrorCode_PatternError
instance RMarshal.FromJSRef GlobalizationErrorCode where
  fromJSRef = RInternal.js_fromEnum

newtype Value a = Value
  { value :: a
  } deriving (Eq, Ord, Show, Read)
instance (RDefault.Default a) => RDefault.Default (Value a) where def = Value RDefault.def
instance (RMarshal.ToJSRef a) => RMarshal.ToJSRef (Value a) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "value" value
    return obj
instance (RMarshal.FromJSRef a) => RMarshal.FromJSRef (Value a) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp "value" obj
    return $ Value RApp.<$> _x0

foreign import javascript interruptible
  "navigator.globalization.getPreferredLanguage(hs_good($c), hs_error($c));"
  js_getPreferredLanguage ::  IO (RInternal.JSEitherRef GlobalizationError (Value String))
getPreferredLanguage ::  IO (Either GlobalizationError (Value String))
getPreferredLanguage  =  do
  res <- js_getPreferredLanguage 
  RInternal.fromJSEitherRef res
