{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.Dialogs
( alert
, confirm
, prompt
, beep
) where

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

foreign import javascript interruptible
  "navigator.notification.alert($1, $c, $2, $3);"
  js_alert :: JSString -> JSString -> JSString -> IO Int

alert :: String -> Maybe String -> Maybe String -> IO Int
alert msg title btn = let
  msg'   = toJSString msg
  title' = maybe jsUndefined toJSString title
  btn'   = maybe jsUndefined toJSString btn
  in js_alert msg' title' btn'

foreign import javascript interruptible
  "navigator.notification.confirm($1, $c, $2, $3);"
  js_confirm :: JSString -> JSString -> JSRef [String] -> IO Int

confirm :: String -> Maybe String -> Maybe [String] -> IO Int
confirm msg title btns = let
  msg'   = toJSString msg
  title' = maybe jsUndefined toJSString title
  in do
    btns' <- maybe (return jsUndefined) toJSRef btns
    js_confirm msg' title' btns'

foreign import javascript interruptible
  "navigator.notification.prompt($1, $c, $2, $3, $4);"
  js_prompt
  :: JSString -> JSString -> JSRef [String] -> JSString -> IO (JSRef ())

prompt ::
  String -> Maybe String -> Maybe [String] -> Maybe String -> IO (Int, String)
prompt msg title btns def = let
  msg'   = toJSString msg
  title' = maybe jsUndefined toJSString title
  def'   = maybe jsUndefined toJSString def
  in do
    btns'    <- maybe (return jsUndefined) toJSRef btns
    obj      <- js_prompt msg' title' btns' def'
    Just i   <- getProp "buttonIndex" obj >>= fromJSRef
    Just str <- getProp "input1"      obj >>= fromJSRef
    return (i, str)

foreign import javascript unsafe
  "navigator.notification.beep($1);"
  beep :: Int -> IO ()
