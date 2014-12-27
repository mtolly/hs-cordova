{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HTMLT
( HTMLT(..)
, Element
, headElement
, body
, runHTMLT
, inside
, getElement
, text
, (</)
, (<-/)
, ($=)
, onclick
, setHTML
, getValue
, style
) where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Reader
import GHCJS.Types
import GHCJS.Foreign

data Element_
type Element = JSRef Element_

newtype HTMLT m a = HTMLT { unHTMLT :: ReaderT Element m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

foreign import javascript unsafe
  "document.getElementsByTagName('body')[0]"
  body :: Element

foreign import javascript unsafe
  "document.getElementsByTagName('head')[0]"
  headElement :: Element

runHTMLT :: (Monad m) => Element -> HTMLT m a -> m a
runHTMLT elt act = runReaderT (unHTMLT act) elt

inside :: (Monad m) => Element -> HTMLT m a -> HTMLT m a
inside elt act = HTMLT $ withReaderT (const elt) $ unHTMLT act

getElement :: (Monad m) => HTMLT m Element
getElement = HTMLT ask

text :: (MonadIO m) => String -> HTMLT m ()
text s = getElement >>= liftIO . appendHTML (toJSString s)

(</) :: (MonadIO m) => String -> HTMLT m a -> HTMLT m a
tag </ sub = do
  child  <- liftIO $ createElement $ toJSString tag
  result <- inside child sub
  parent <- getElement
  liftIO $ appendChild child parent
  return result
infixr 0 </

(<-/) :: (MonadIO m) => String -> HTMLT m () -> HTMLT m Element
tag <-/ sub = tag </ (sub >> getElement)
infixr 0 <-/

foreign import javascript unsafe
  "$2.appendChild($1);"
  appendChild :: Element -> Element -> IO ()

foreign import javascript unsafe
  "document.createElement($1)"
  createElement :: JSString -> IO Element

foreign import javascript unsafe
  "$3.setAttribute($1, $2);"
  setAttribute :: JSString -> JSString -> Element -> IO ()

($=) :: (MonadIO m) => String -> String -> HTMLT m ()
k $= v = getElement >>= liftIO . setAttribute (toJSString k) (toJSString v)
infixr 0 $=

foreign import javascript unsafe
  "$2.innerHTML = $1;"
  js_setHTML :: JSString -> Element -> IO ()

setHTML :: (MonadIO m) => String -> HTMLT m ()
setHTML s = getElement >>= liftIO . js_setHTML (toJSString s)

foreign import javascript unsafe
  "$2.innerHTML += $1;"
  appendHTML :: JSString -> Element -> IO ()

foreign import javascript unsafe
  "$1.value"
  js_getValue :: Element -> IO JSString

getValue :: (MonadIO m) => HTMLT m String
getValue = liftM fromJSString $ getElement >>= liftIO . js_getValue

foreign import javascript unsafe
  "$1.onclick = $2;"
  js_onclick :: Element -> JSFun (IO ()) -> IO ()

onclick :: (MonadIO m) => IO () -> HTMLT m ()
onclick act = do
  elt <- getElement
  liftIO $ do
    cb <- asyncCallback (DomRetain $ castRef elt) act
    js_onclick elt cb

style :: (MonadIO m) => String -> [(String, String)] -> HTMLT m ()
style ctxt kvs = "style" </ do
  "type" $= "text/css"
  let rules = unwords [ k ++ ": " ++ v ++ ";" | (k, v) <- kvs ]
  text $ ctxt ++ " { " ++ rules ++ " } "
