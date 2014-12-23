
{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.StatusBar
( overlaysWebView
, styleDefault, styleLightContent, styleBlackTranslucent, styleBlackOpaque
, backgroundColorByName, backgroundColorByHexString
, hideBar, showBar, isVisible
) where

import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified System.Cordova.Internal as RInternal


foreign import javascript unsafe
  "StatusBar.overlaysWebView($1);"
  js_overlaysWebView :: RTypes.JSRef (Bool) -> IO (RTypes.JSRef (()))
overlaysWebView :: Bool -> IO (())
overlaysWebView arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_overlaysWebView arg0'
  RInternal.fromJSRef' res


foreign import javascript unsafe
  "StatusBar.styleDefault();"
  js_styleDefault ::  IO (RTypes.JSRef (()))
styleDefault ::  IO (())
styleDefault  =  do
  res <- js_styleDefault 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "StatusBar.styleLightContent();"
  js_styleLightContent ::  IO (RTypes.JSRef (()))
styleLightContent ::  IO (())
styleLightContent  =  do
  res <- js_styleLightContent 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "StatusBar.styleBlackTranslucent();"
  js_styleBlackTranslucent ::  IO (RTypes.JSRef (()))
styleBlackTranslucent ::  IO (())
styleBlackTranslucent  =  do
  res <- js_styleBlackTranslucent 
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "StatusBar.styleBlackOpaque();"
  js_styleBlackOpaque ::  IO (RTypes.JSRef (()))
styleBlackOpaque ::  IO (())
styleBlackOpaque  =  do
  res <- js_styleBlackOpaque 
  RInternal.fromJSRef' res



foreign import javascript unsafe
  "StatusBar.backgroundColorByName($1);"
  js_backgroundColorByName :: RTypes.JSRef (String) -> IO (RTypes.JSRef (()))
backgroundColorByName :: String -> IO (())
backgroundColorByName arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_backgroundColorByName arg0'
  RInternal.fromJSRef' res

foreign import javascript unsafe
  "StatusBar.backgroundColorByHexString($1);"
  js_backgroundColorByHexString :: RTypes.JSRef (String) -> IO (RTypes.JSRef (()))
backgroundColorByHexString :: String -> IO (())
backgroundColorByHexString arg0 =  do
  arg0' <- RMarshal.toJSRef arg0
  res <- js_backgroundColorByHexString arg0'
  RInternal.fromJSRef' res


foreign import javascript unsafe
  "StatusBar.hide();"
  js_hideBar ::  IO (RTypes.JSRef (()))
hideBar ::  IO (())
hideBar  =  do
  res <- js_hideBar 
  RInternal.fromJSRef' res
foreign import javascript unsafe
  "StatusBar.show();"
  js_showBar ::  IO (RTypes.JSRef (()))
showBar ::  IO (())
showBar  =  do
  res <- js_showBar 
  RInternal.fromJSRef' res
foreign import javascript unsafe
  "StatusBar.isVisible"
  js_isVisible ::  IO (RTypes.JSRef (Bool))
isVisible ::  IO (Bool)
isVisible  =  do
  res <- js_isVisible 
  RInternal.fromJSRef' res
