module System.Cordova.NetworkInformation
( Connection(..)
, connectionType
) where

import GHCJS.Types
import GHCJS.Marshal
import Data.Maybe (fromMaybe)

import System.Cordova.NetworkInformation.Connection

foreign import javascript unsafe
  "navigator.connection.type"
  js_connectionType :: IO (JSRef Connection)

connectionType :: IO Connection
connectionType = js_connectionType >>= fmap (fromMaybe Unknown) . fromJSRef
