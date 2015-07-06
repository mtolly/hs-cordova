
{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Globalization
( GlobalizationError(..)
, GlobalizationErrorCode(..)
, Value(..)
, getPreferredLanguage
, getLocaleName
, getFirstDayOfWeek
, DST(..), isDayLightSavingsTime
, NumStrOptions(..) , NumType(..), stringToNumber, numberToString
, DateStrOptions(..), FormatLength(..), Selector(..), dateToString, stringToDate
, NameType(..), Item(..), DateNameOptions(..), getDateNames
, CurrencyPattern(..), getCurrencyPattern
, DatePattern(..), getDatePattern
, NumberPattern(..), getNumberPattern
) where

import Data.Time
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified Data.Default as RDefault
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp
import qualified Data.Text as RText


data GlobalizationError = GlobalizationError
  { code :: GlobalizationErrorCode
  , message :: T.Text
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
    _x0 <- RInternal.fromProp (RText.pack "code") obj
    _x1 <- RInternal.fromProp (RText.pack "message") obj
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
    _x0 <- RInternal.fromProp (RText.pack "value") obj
    return $ Value RApp.<$> _x0

foreign import javascript interruptible
  "navigator.globalization.getPreferredLanguage(hs_good($c), hs_error($c));"
  js_getPreferredLanguage ::  IO (RInternal.JSEitherRef GlobalizationError (Value T.Text))
getPreferredLanguage ::  IO (Either GlobalizationError (Value T.Text))
getPreferredLanguage  =  do
  res <- js_getPreferredLanguage 
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "navigator.globalization.getLocaleName(hs_good($c), hs_error($c));"
  js_getLocaleName ::  IO (RInternal.JSEitherRef GlobalizationError (Value T.Text))
getLocaleName ::  IO (Either GlobalizationError (Value T.Text))
getLocaleName  =  do
  res <- js_getLocaleName 
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "navigator.globalization.dateToString($1, hs_good($c), hs_error($c), $2);"
  js_dateToString :: RTypes.JSRef (UTCTime) -> RTypes.JSRef (DateStrOptions) -> IO (RInternal.JSEitherRef GlobalizationError (Value T.Text))
dateToString :: UTCTime -> DateStrOptions -> IO (Either GlobalizationError (Value T.Text))
dateToString arg0 arg1 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  res <- js_dateToString arg0' arg1'
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "navigator.globalization.getCurrencyPattern($1, hs_good($c), hs_error($c));"
  js_getCurrencyPattern :: RTypes.JSRef (T.Text) -> IO (RInternal.JSEitherRef GlobalizationError CurrencyPattern)
getCurrencyPattern :: T.Text -> IO (Either GlobalizationError CurrencyPattern)
getCurrencyPattern arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_getCurrencyPattern arg0'
  RInternal.fromJSEitherRef res

data CurrencyPattern = CurrencyPattern
  { cPattern :: T.Text
  , cCode :: T.Text
  , cFraction :: Int
  , cRounding :: Double
  , cDecimal :: T.Text
  , cGrouping :: T.Text
  } deriving (Eq, Ord, Show, Read)
instance  RMarshal.ToJSRef (CurrencyPattern) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "pattern" cPattern
    _set "code" cCode
    _set "fraction" cFraction
    _set "rounding" cRounding
    _set "decimal" cDecimal
    _set "grouping" cGrouping
    return obj
instance  RMarshal.FromJSRef (CurrencyPattern) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "pattern") obj
    _x1 <- RInternal.fromProp (RText.pack "code") obj
    _x2 <- RInternal.fromProp (RText.pack "fraction") obj
    _x3 <- RInternal.fromProp (RText.pack "rounding") obj
    _x4 <- RInternal.fromProp (RText.pack "decimal") obj
    _x5 <- RInternal.fromProp (RText.pack "grouping") obj
    return $ CurrencyPattern RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2 RApp.<*> _x3 RApp.<*> _x4 RApp.<*> _x5

data NameType
  = Narrow
  | Wide
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "\"narrow\"" _NameType_Narrow :: RTypes.JSRef NameType
foreign import javascript unsafe "\"wide\"" _NameType_Wide :: RTypes.JSRef NameType
instance RMarshal.ToJSRef NameType where
  toJSRef Narrow = return _NameType_Narrow
  toJSRef Wide = return _NameType_Wide
instance RMarshal.FromJSRef NameType where
  fromJSRef = RInternal.js_fromEnum

data Item
  = Months
  | Days
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "\"months\"" _Item_Months :: RTypes.JSRef Item
foreign import javascript unsafe "\"days\"" _Item_Days :: RTypes.JSRef Item
instance RMarshal.ToJSRef Item where
  toJSRef Months = return _Item_Months
  toJSRef Days = return _Item_Days
instance RMarshal.FromJSRef Item where
  fromJSRef = RInternal.js_fromEnum

data DateNameOptions = DateNameOptions
  { nameType :: Maybe NameType
  , item :: Maybe Item
  } deriving (Eq, Ord, Show, Read)
instance  RDefault.Default (DateNameOptions) where def = DateNameOptions RDefault.def RDefault.def
instance  RMarshal.ToJSRef (DateNameOptions) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "type" nameType
    _setJust "item" item
    return obj
instance  RMarshal.FromJSRef (DateNameOptions) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "type") obj
    _x1 <- RInternal.fromProp (RText.pack "item") obj
    return $ DateNameOptions RApp.<$> _x0 RApp.<*> _x1

foreign import javascript interruptible
  "navigator.globalization.getDateNames(hs_good($c), hs_error($c), $1);"
  js_getDateNames :: RTypes.JSRef (DateNameOptions) -> IO (RInternal.JSEitherRef GlobalizationError (Value [T.Text]))
getDateNames :: DateNameOptions -> IO (Either GlobalizationError (Value [T.Text]))
getDateNames arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_getDateNames arg0'
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "navigator.globalization.getDatePattern(hs_good($c), hs_error($c), $1);"
  js_getDatePattern :: RTypes.JSRef (DateStrOptions) -> IO (RInternal.JSEitherRef GlobalizationError DatePattern)
getDatePattern :: DateStrOptions -> IO (Either GlobalizationError DatePattern)
getDatePattern arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_getDatePattern arg0'
  RInternal.fromJSEitherRef res

data DatePattern = DatePattern
  { dPattern :: T.Text
  , dTimezone :: T.Text
  , dUTCOffset :: Double
  , dDSTOffset :: Double
  } deriving (Eq, Ord, Show, Read)
instance  RMarshal.ToJSRef (DatePattern) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "pattern" dPattern
    _set "timezone" dTimezone
    _set "utc_offset" dUTCOffset
    _set "dst_offset" dDSTOffset
    return obj
instance  RMarshal.FromJSRef (DatePattern) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "pattern") obj
    _x1 <- RInternal.fromProp (RText.pack "timezone") obj
    _x2 <- RInternal.fromProp (RText.pack "utc_offset") obj
    _x3 <- RInternal.fromProp (RText.pack "dst_offset") obj
    return $ DatePattern RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2 RApp.<*> _x3

foreign import javascript interruptible
  "navigator.globalization.getFirstDayOfWeek(hs_good($c), hs_error($c));"
  js_getFirstDayOfWeek ::  IO (RInternal.JSEitherRef GlobalizationError (Value Int))
getFirstDayOfWeek ::  IO (Either GlobalizationError (Value Int))
getFirstDayOfWeek  =  do
  res <- js_getFirstDayOfWeek 
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "navigator.globalization.getNumberPattern(hs_good($c), hs_error($c), $1);"
  js_getNumberPattern :: RTypes.JSRef (NumStrOptions) -> IO (RInternal.JSEitherRef GlobalizationError NumberPattern)
getNumberPattern :: NumStrOptions -> IO (Either GlobalizationError NumberPattern)
getNumberPattern arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_getNumberPattern arg0'
  RInternal.fromJSEitherRef res

data NumberPattern = NumberPattern
  { nPattern :: T.Text
  , nSymbol :: T.Text
  , nFraction :: Int
  , nRounding :: Double
  , nPositive :: T.Text
  , nNegative :: T.Text
  , nDecimal :: T.Text
  , nGrouping :: T.Text
  } deriving (Eq, Ord, Show, Read)
instance  RMarshal.ToJSRef (NumberPattern) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "pattern" nPattern
    _set "symbol" nSymbol
    _set "fraction" nFraction
    _set "rounding" nRounding
    _set "positive" nPositive
    _set "negative" nNegative
    _set "decimal" nDecimal
    _set "grouping" nGrouping
    return obj
instance  RMarshal.FromJSRef (NumberPattern) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "pattern") obj
    _x1 <- RInternal.fromProp (RText.pack "symbol") obj
    _x2 <- RInternal.fromProp (RText.pack "fraction") obj
    _x3 <- RInternal.fromProp (RText.pack "rounding") obj
    _x4 <- RInternal.fromProp (RText.pack "positive") obj
    _x5 <- RInternal.fromProp (RText.pack "negative") obj
    _x6 <- RInternal.fromProp (RText.pack "decimal") obj
    _x7 <- RInternal.fromProp (RText.pack "grouping") obj
    return $ NumberPattern RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2 RApp.<*> _x3 RApp.<*> _x4 RApp.<*> _x5 RApp.<*> _x6 RApp.<*> _x7

newtype DST = DST
  { dst :: Bool
  } deriving (Eq, Ord, Show, Read)
instance  RMarshal.ToJSRef (DST) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "dst" dst
    return obj
instance  RMarshal.FromJSRef (DST) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "dst") obj
    return $ DST RApp.<$> _x0

foreign import javascript interruptible
  "navigator.globalization.isDayLightSavingsTime($1, hs_good($c), hs_error($c));"
  js_isDayLightSavingsTime :: RTypes.JSRef (UTCTime) -> IO (RInternal.JSEitherRef GlobalizationError DST)
isDayLightSavingsTime :: UTCTime -> IO (Either GlobalizationError DST)
isDayLightSavingsTime arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_isDayLightSavingsTime arg0'
  RInternal.fromJSEitherRef res

foreign import javascript interruptible
  "navigator.globalization.numberToString($1, hs_good($c), hs_error($c), $2);"
  js_numberToString :: RTypes.JSRef (Double) -> RTypes.JSRef (NumStrOptions) -> IO (RInternal.JSEitherRef GlobalizationError (Value T.Text))
numberToString :: Double -> NumStrOptions -> IO (Either GlobalizationError (Value T.Text))
numberToString arg0 arg1 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  res <- js_numberToString arg0' arg1'
  RInternal.fromJSEitherRef res

data FormatLength
  = Short
  | Medium
  | Long
  | Full
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "\"short\"" _FormatLength_Short :: RTypes.JSRef FormatLength
foreign import javascript unsafe "\"medium\"" _FormatLength_Medium :: RTypes.JSRef FormatLength
foreign import javascript unsafe "\"long\"" _FormatLength_Long :: RTypes.JSRef FormatLength
foreign import javascript unsafe "\"full\"" _FormatLength_Full :: RTypes.JSRef FormatLength
instance RMarshal.ToJSRef FormatLength where
  toJSRef Short = return _FormatLength_Short
  toJSRef Medium = return _FormatLength_Medium
  toJSRef Long = return _FormatLength_Long
  toJSRef Full = return _FormatLength_Full
instance RMarshal.FromJSRef FormatLength where
  fromJSRef = RInternal.js_fromEnum

data Selector
  = Date
  | Time
  | DateAndTime
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "\"date\"" _Selector_Date :: RTypes.JSRef Selector
foreign import javascript unsafe "\"time\"" _Selector_Time :: RTypes.JSRef Selector
foreign import javascript unsafe "\"date and time\"" _Selector_DateAndTime :: RTypes.JSRef Selector
instance RMarshal.ToJSRef Selector where
  toJSRef Date = return _Selector_Date
  toJSRef Time = return _Selector_Time
  toJSRef DateAndTime = return _Selector_DateAndTime
instance RMarshal.FromJSRef Selector where
  fromJSRef = RInternal.js_fromEnum

data DateStrOptions = DateStrOptions
  { formatLength :: Maybe FormatLength
  , selector :: Maybe Selector
  } deriving (Eq, Ord, Show, Read)
instance  RDefault.Default (DateStrOptions) where def = DateStrOptions RDefault.def RDefault.def
instance  RMarshal.ToJSRef (DateStrOptions) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "formatLength" formatLength
    _setJust "selector" selector
    return obj
instance  RMarshal.FromJSRef (DateStrOptions) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "formatLength") obj
    _x1 <- RInternal.fromProp (RText.pack "selector") obj
    return $ DateStrOptions RApp.<$> _x0 RApp.<*> _x1

data CordovaTime = CordovaTime
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  , second :: Int
  , millisecond :: Maybe Int
  } deriving (Eq, Ord, Show, Read)
instance  RDefault.Default (CordovaTime) where def = CordovaTime RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def RDefault.def
instance  RMarshal.ToJSRef (CordovaTime) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "year" year
    _set "month" month
    _set "day" day
    _set "hour" hour
    _set "minute" minute
    _set "second" second
    _setJust "millisecond" millisecond
    return obj
instance  RMarshal.FromJSRef (CordovaTime) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "year") obj
    _x1 <- RInternal.fromProp (RText.pack "month") obj
    _x2 <- RInternal.fromProp (RText.pack "day") obj
    _x3 <- RInternal.fromProp (RText.pack "hour") obj
    _x4 <- RInternal.fromProp (RText.pack "minute") obj
    _x5 <- RInternal.fromProp (RText.pack "second") obj
    _x6 <- RInternal.fromProp (RText.pack "millisecond") obj
    return $ CordovaTime RApp.<$> _x0 RApp.<*> _x1 RApp.<*> _x2 RApp.<*> _x3 RApp.<*> _x4 RApp.<*> _x5 RApp.<*> _x6

foreign import javascript interruptible
  "navigator.globalization.stringToDate($1, hs_good($c), hs_error($c), $2);"
  js_stringToDate_cordova :: RTypes.JSRef (T.Text) -> RTypes.JSRef (DateStrOptions) -> IO (RInternal.JSEitherRef GlobalizationError CordovaTime)
stringToDate_cordova :: T.Text -> DateStrOptions -> IO (Either GlobalizationError CordovaTime)
stringToDate_cordova arg0 arg1 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  res <- js_stringToDate_cordova arg0' arg1'
  RInternal.fromJSEitherRef res

stringToDate :: T.Text -> DateStrOptions -> IO (Either GlobalizationError LocalTime)
stringToDate s opts = fmap (fmap corToHs) $ stringToDate_cordova s opts where
  corToHs cortime = let
    tod = TimeOfDay (hour cortime) (minute cortime) secs
    secs = fromIntegral (second cortime) +
      fromIntegral (fromMaybe 0 $ millisecond cortime) / 1000
    localday = fromGregorian (fromIntegral $ year cortime) (month cortime) (day cortime)
    in LocalTime localday tod

newtype NumStrOptions = NumStrOptions
  { numType :: Maybe NumType
  } deriving (Eq, Ord, Show, Read)
instance  RDefault.Default (NumStrOptions) where def = NumStrOptions RDefault.def
instance  RMarshal.ToJSRef (NumStrOptions) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _setJust "type" numType
    return obj
instance  RMarshal.FromJSRef (NumStrOptions) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "type") obj
    return $ NumStrOptions RApp.<$> _x0

data NumType
  = Decimal
  | Percent
  | Currency
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "\"decimal\"" _NumType_Decimal :: RTypes.JSRef NumType
foreign import javascript unsafe "\"percent\"" _NumType_Percent :: RTypes.JSRef NumType
foreign import javascript unsafe "\"currency\"" _NumType_Currency :: RTypes.JSRef NumType
instance RMarshal.ToJSRef NumType where
  toJSRef Decimal = return _NumType_Decimal
  toJSRef Percent = return _NumType_Percent
  toJSRef Currency = return _NumType_Currency
instance RMarshal.FromJSRef NumType where
  fromJSRef = RInternal.js_fromEnum

foreign import javascript interruptible
  "navigator.globalization.stringToNumber($1, hs_good($c), hs_error($c), $2);"
  js_stringToNumber :: RTypes.JSRef (T.Text) -> RTypes.JSRef (NumStrOptions) -> IO (RInternal.JSEitherRef GlobalizationError (Value Double))
stringToNumber :: T.Text -> NumStrOptions -> IO (Either GlobalizationError (Value Double))
stringToNumber arg0 arg1 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  res <- js_stringToNumber arg0' arg1'
  RInternal.fromJSEitherRef res
