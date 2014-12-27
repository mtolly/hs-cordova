
{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Dialogs
( alert
, confirm
, PromptResult(..)
, prompt
, beep
) where

import qualified Data.Text as T
import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified GHCJS.Foreign as RForeign
import qualified System.Cordova.Internal as RInternal
import qualified Control.Applicative as RApp
import qualified Data.Text as RText


foreign import javascript interruptible
  "navigator.notification.alert($1, $c, $2, $3);"
  js_alert :: RTypes.JSRef (T.Text) -> RTypes.JSRef (Maybe T.Text) -> RTypes.JSRef (Maybe T.Text) -> IO (RTypes.JSRef (Int))
alert :: T.Text -> Maybe T.Text -> Maybe T.Text -> IO (Int)
alert arg0 arg1 arg2 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  arg2' <- RMarshal.toJSRef arg2
  res <- js_alert arg0' arg1' arg2'
  RInternal.fromJSRef' res

foreign import javascript interruptible
  "navigator.notification.confirm($1, $c, $2, $3);"
  js_confirm :: RTypes.JSRef (T.Text) -> RTypes.JSRef (Maybe T.Text) -> RTypes.JSRef (Maybe [T.Text]) -> IO (RTypes.JSRef (Int))
confirm :: T.Text -> Maybe T.Text -> Maybe [T.Text] -> IO (Int)
confirm arg0 arg1 arg2 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  arg2' <- RMarshal.toJSRef arg2
  res <- js_confirm arg0' arg1' arg2'
  RInternal.fromJSRef' res

data PromptResult = PromptResult
  { buttonIndex :: Int
  , input1 :: T.Text
  } deriving (Eq, Ord, Show, Read)
instance  RMarshal.ToJSRef (PromptResult) where
  toJSRef opts = do
    obj <- RForeign.newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> RMarshal.toJSRef x >>= \ref -> RForeign.setProp s ref obj
        _set s f = RMarshal.toJSRef (f opts) >>= \ref -> RForeign.setProp s ref obj
    _set "buttonIndex" buttonIndex
    _set "input1" input1
    return obj
instance  RMarshal.FromJSRef (PromptResult) where
  fromJSRef obj = do
    _x0 <- RInternal.fromProp (RText.pack "buttonIndex") obj
    _x1 <- RInternal.fromProp (RText.pack "input1") obj
    return $ PromptResult RApp.<$> _x0 RApp.<*> _x1

foreign import javascript interruptible
  "navigator.notification.prompt($1, $c, $2, $3, $4);"
  js_prompt :: RTypes.JSRef (T.Text) -> RTypes.JSRef (Maybe T.Text) -> RTypes.JSRef (Maybe [T.Text]) -> RTypes.JSRef (Maybe T.Text) -> IO (RTypes.JSRef (PromptResult))
prompt :: T.Text -> Maybe T.Text -> Maybe [T.Text] -> Maybe T.Text -> IO (PromptResult)
prompt arg0 arg1 arg2 arg3 =  do
  arg0' <- RMarshal.toJSRef arg0
  arg1' <- RMarshal.toJSRef arg1
  arg2' <- RMarshal.toJSRef arg2
  arg3' <- RMarshal.toJSRef arg3
  res <- js_prompt arg0' arg1' arg2' arg3'
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "navigator.notification.beep($1);"
  js_beep :: RTypes.JSRef (Int) -> IO (RTypes.JSRef (()))
beep :: Int -> IO (())
beep arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_beep arg0'
  RInternal.fromJSRef' res
