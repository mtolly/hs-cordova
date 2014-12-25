{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HTMLT
( HTMLT(..)
, Element
, body
, runHTMLT
, text
, (</), (<-/)
, onclick
, getElement
, ($=)
, setHTML
, setAttribute
, getValue
) where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Trans.Reader
import GHCJS.Types
import GHCJS.Foreign

data Element_
type Element = JSRef Element_

newtype HTMLT m a = HTMLT { unHTMLT :: ReaderT Element m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

foreign import javascript unsafe
  "document.getElementsByTagName('body')[0]"
  body :: Element

runHTMLT :: (Monad m) => Element -> HTMLT m a -> m a
runHTMLT elt act = runReaderT (unHTMLT act) elt

getElement :: (Monad m) => HTMLT m Element
getElement = HTMLT ask

text :: (MonadIO m) => String -> HTMLT m ()
text s = getElement >>= liftIO . appendHTML (toJSString s)

(</) :: (MonadIO m) => String -> HTMLT m a -> HTMLT m a
tag </ sub = do
  child  <- liftIO $ createElement $ toJSString tag
  result <- HTMLT $ withReaderT (const child) $ unHTMLT sub
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
  setHTML :: JSString -> Element -> IO ()

foreign import javascript unsafe
  "$2.innerHTML += $1;"
  appendHTML :: JSString -> Element -> IO ()

foreign import javascript unsafe
  "$1.value"
  getValue :: Element -> IO JSString

foreign import javascript unsafe
  "$1.onclick = $2;"
  js_onclick :: Element -> JSFun (IO ()) -> IO ()

onclick' :: Element -> IO () -> IO ()
onclick' elt fn = do
  cb <- asyncCallback (DomRetain $ castRef elt) fn
  js_onclick elt cb

onclick :: (MonadIO m) => IO () -> HTMLT m ()
onclick act = getElement >>= \e -> liftIO $ onclick' e act
