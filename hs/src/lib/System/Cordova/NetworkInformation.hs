module System.Cordova.NetworkInformation
( Connection(..)
, connectionType
) where

import GHCJS.Types
import GHCJS.Marshal
import Data.Maybe (fromMaybe)
import System.Cordova.Internal



data Connection = Unknown | Ethernet | Wifi | Cell2G | Cell3G | Cell4G | Cell | None deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Connection.UNKNOWN" _Connection_Unknown :: JSRef Connection
foreign import javascript unsafe "Connection.ETHERNET" _Connection_Ethernet :: JSRef Connection
foreign import javascript unsafe "Connection.WIFI" _Connection_Wifi :: JSRef Connection
foreign import javascript unsafe "Connection.CELL_2G" _Connection_Cell2G :: JSRef Connection
foreign import javascript unsafe "Connection.CELL_3G" _Connection_Cell3G :: JSRef Connection
foreign import javascript unsafe "Connection.CELL_4G" _Connection_Cell4G :: JSRef Connection
foreign import javascript unsafe "Connection.CELL" _Connection_Cell :: JSRef Connection
foreign import javascript unsafe "Connection.NONE" _Connection_None :: JSRef Connection
instance ToJSRef Connection where
  toJSRef Unknown = return _Connection_Unknown
  toJSRef Ethernet = return _Connection_Ethernet
  toJSRef Wifi = return _Connection_Wifi
  toJSRef Cell2G = return _Connection_Cell2G
  toJSRef Cell3G = return _Connection_Cell3G
  toJSRef Cell4G = return _Connection_Cell4G
  toJSRef Cell = return _Connection_Cell
  toJSRef None = return _Connection_None
instance FromJSRef Connection where
  fromJSRef = js_fromEnum

foreign import javascript unsafe
  "navigator.connection.type"
  js_connectionType :: IO (JSRef Connection)

connectionType :: IO Connection
connectionType = js_connectionType >>= fmap (fromMaybe Unknown) . fromJSRef
