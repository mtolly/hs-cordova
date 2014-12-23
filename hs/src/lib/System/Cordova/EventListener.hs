module System.Cordova.EventListener where

import GHCJS.Foreign
import GHCJS.Types

foreign import javascript unsafe
  "window.document"
  document :: JSRef e

foreign import javascript unsafe
  "(function(){ return window; })()"
  window :: JSRef e

foreign import javascript unsafe
  "$3.addEventListener($1, $2);"
  js_addEventListener :: JSString -> JSFun f-> JSRef e -> IO ()

foreign import javascript unsafe
  "$3.removeEventListener($1, $2);"
  js_removeEventListener :: JSString -> JSFun f -> JSRef e -> IO ()

addEventListener :: String -> IO () -> JSRef e -> IO (IO ())
addEventListener str f elt = do
  jsfun <- asyncCallback (DomRetain $ castRef elt) f
  js_addEventListener (toJSString str) jsfun elt
  return $ do
    js_removeEventListener (toJSString str) jsfun elt
    releaseAll jsfun

addEventListener1 :: String -> (JSRef a -> IO ()) -> JSRef e -> IO (IO ())
addEventListener1 str f elt = do
  jsfun <- asyncCallback1 (DomRetain $ castRef elt) f
  js_addEventListener (toJSString str) jsfun elt
  return $ do
    js_removeEventListener (toJSString str) jsfun elt
    releaseAll jsfun
