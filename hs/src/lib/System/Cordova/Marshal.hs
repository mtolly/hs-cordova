module System.Cordova.Marshal
( Marshal(..)
, Unmarshal(..)
, unmarshal'
) where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Control.Monad (forM_)
import Data.Int
import Data.Word

class Marshal a where
  marshal :: a -> IO (JSRef a)
  marshalList :: [a] -> IO (JSRef [a])
  marshalList xs = do
    ary <- newArray
    forM_ xs $ \x -> do
      r <- marshal x
      pushArray r ary
    return $ castRef ary

class Unmarshal a where
  unmarshal :: JSRef a -> IO (Either String a)
  unmarshalList :: JSRef [a] -> IO (Either String [a])
  unmarshalList ary = do
    let ary' = castRef ary
    len <- lengthArray ary'
    refs <- mapM (`indexArray` ary') [0 .. len - 1]
    fmap sequence $ mapM unmarshal refs

unmarshal' :: (Unmarshal a) => JSRef a -> IO a
unmarshal' r = unmarshal r >>= either error return

unmarshalFromJSRef :: (FromJSRef a) => String -> JSRef a -> IO (Either String a)
unmarshalFromJSRef typ r = fromJSRef r >>= \mx -> return $ case mx of
  Nothing -> Left $ "unmarshal: couldn't read value as " ++ typ
  Just x  -> Right x

instance Marshal Int where marshal = toJSRef
instance Unmarshal Int where
  unmarshal = unmarshalFromJSRef "Int"
instance Marshal Int8 where marshal = toJSRef
instance Unmarshal Int8 where
  unmarshal = unmarshalFromJSRef "Int8"
instance Marshal Int16 where marshal = toJSRef
instance Unmarshal Int16 where
  unmarshal = unmarshalFromJSRef "Int16"
instance Marshal Int32 where marshal = toJSRef
instance Unmarshal Int32 where
  unmarshal = unmarshalFromJSRef "Int32"
instance Marshal Word where marshal = toJSRef
instance Unmarshal Word where
  unmarshal = unmarshalFromJSRef "Word"
instance Marshal Word8 where marshal = toJSRef
instance Unmarshal Word8 where
  unmarshal = unmarshalFromJSRef "Word8"
instance Marshal Word16 where marshal = toJSRef
instance Unmarshal Word16 where
  unmarshal = unmarshalFromJSRef "Word16"
instance Marshal Word32 where marshal = toJSRef
instance Unmarshal Word32 where
  unmarshal = unmarshalFromJSRef "Word32"
instance Marshal Bool where marshal = toJSRef
instance Unmarshal Bool where
  unmarshal = unmarshalFromJSRef "Bool"

instance (Marshal a) => Marshal [a] where
  marshal = marshalList
instance (Unmarshal a) => Unmarshal [a] where
  unmarshal = unmarshalList

instance Marshal Char where
  marshal = toJSRef
  marshalList = toJSRefListOf
instance Unmarshal Char where
  unmarshal = unmarshalFromJSRef "Char"
  unmarshalList = unmarshalFromJSRef "String"

instance (Marshal a) => Marshal (Maybe a) where
  marshal Nothing  = return jsNull
  marshal (Just x) = fmap castRef $ marshal x
instance (Unmarshal a) => Unmarshal (Maybe a) where
  unmarshal r = if eqRef r jsNull || eqRef r jsUndefined
    then return $ Right Nothing
    else fmap (fmap Just) $ unmarshal $ castRef r
