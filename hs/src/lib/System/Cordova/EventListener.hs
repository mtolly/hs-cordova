module System.Cordova.EventListener where

import GHCJS.Foreign
import GHCJS.Types

foreign import javascript unsafe
  "window.document"
  document :: JSRef a

foreign import javascript unsafe
  "$3.addEventListener($1, $2);"
  js_addEventListener :: JSString -> JSFun (IO ()) -> JSRef a -> IO ()

foreign import javascript unsafe
  "$3.removeEventListener($1, $2);"
  js_removeEventListener :: JSString -> JSFun (IO ()) -> JSRef a -> IO ()

addEventListener_ :: String -> IO () -> JSRef a -> IO (IO ())
addEventListener_ str f elt = do
  jsfun <- asyncCallback AlwaysRetain f
  js_addEventListener (toJSString str) jsfun elt
  return $ do
    js_removeEventListener (toJSString str) jsfun elt
    releaseAll jsfun
