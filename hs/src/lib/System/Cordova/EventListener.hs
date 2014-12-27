module System.Cordova.EventListener where

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Control.Monad ((>=>))
import System.Cordova.Internal (fromJSRef')

foreign import javascript unsafe
  "window.document"
  document :: JSRef e

foreign import javascript unsafe
  "(function(){ return window; })()"
  window :: JSRef e

foreign import javascript unsafe
  "$3.addEventListener($1, $2);"
  js_addEventListener :: JSString -> JSFun f -> JSRef e -> IO ()

foreign import javascript unsafe
  "$3.removeEventListener($1, $2);"
  js_removeEventListener :: JSString -> JSFun f -> JSRef e -> IO ()

addEventListener :: String -> IO () -> JSRef e -> IO (IO ())
addEventListener str f elt = do
  jsfun <- asyncCallback (DomRetain $ castRef elt) f
  js_addEventListener (toJSString str) jsfun elt
  return $ do
    js_removeEventListener (toJSString str) jsfun elt
    releaseDom elt jsfun

addEventListener1 :: String -> (JSRef a -> IO ()) -> JSRef e -> IO (IO ())
addEventListener1 str f elt = do
  jsfun <- asyncCallback1 (DomRetain $ castRef elt) f
  js_addEventListener (toJSString str) jsfun elt
  return $ do
    js_removeEventListener (toJSString str) jsfun elt
    releaseDom elt jsfun

globalListener
  :: (FromJSRef err, FromJSRef a)
  => (JSFun (JSRef a -> IO ()) -> JSFun (JSRef err -> IO ()) -> IO (JSRef id))
  -> (JSRef id -> IO ())
  -> (Either err a -> IO ())
  -> IO (IO ())
globalListener on off f = do
  fnGood  <- asyncCallback1 AlwaysRetain $ fromJSRef' >=> f . Right
  fnError <- asyncCallback1 AlwaysRetain $ fromJSRef' >=> f . Left
  watchID <- on fnGood fnError
  return $ do
    off watchID
    release fnGood
    release fnError
