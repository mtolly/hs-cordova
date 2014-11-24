
module System.Cordova.NetworkInformation
( Connection(..)
, connectionType
, offlineEvent
, onlineEvent
) where

import System.Cordova.EventListener
import qualified GHCJS.Types as RTypes
import qualified GHCJS.Marshal as RMarshal
import qualified System.Cordova.Internal as RInternal


foreign import javascript unsafe
  "navigator.connection.type"
  js_connectionType ::  IO (RTypes.JSRef (Connection))
connectionType ::  IO (Connection)
connectionType  =  do
  res <- js_connectionType 
  RInternal.fromJSRef' res

data Connection = Unknown | Ethernet | Wifi | Cell2G | Cell3G | Cell4G | Cell | None deriving (Eq, Ord, Show, Read, Enum, Bounded)
foreign import javascript unsafe "Connection.UNKNOWN" _Connection_Unknown :: RTypes.JSRef Connection
foreign import javascript unsafe "Connection.ETHERNET" _Connection_Ethernet :: RTypes.JSRef Connection
foreign import javascript unsafe "Connection.WIFI" _Connection_Wifi :: RTypes.JSRef Connection
foreign import javascript unsafe "Connection.CELL_2G" _Connection_Cell2G :: RTypes.JSRef Connection
foreign import javascript unsafe "Connection.CELL_3G" _Connection_Cell3G :: RTypes.JSRef Connection
foreign import javascript unsafe "Connection.CELL_4G" _Connection_Cell4G :: RTypes.JSRef Connection
foreign import javascript unsafe "Connection.CELL" _Connection_Cell :: RTypes.JSRef Connection
foreign import javascript unsafe "Connection.NONE" _Connection_None :: RTypes.JSRef Connection
instance RMarshal.ToJSRef Connection where
  toJSRef Unknown = return _Connection_Unknown
  toJSRef Ethernet = return _Connection_Ethernet
  toJSRef Wifi = return _Connection_Wifi
  toJSRef Cell2G = return _Connection_Cell2G
  toJSRef Cell3G = return _Connection_Cell3G
  toJSRef Cell4G = return _Connection_Cell4G
  toJSRef Cell = return _Connection_Cell
  toJSRef None = return _Connection_None
instance RMarshal.FromJSRef Connection where
  fromJSRef = RInternal.js_fromEnum

offlineEvent :: IO () -> IO (IO ())
offlineEvent f = addEventListener_ "offline" f document

onlineEvent :: IO () -> IO (IO ())
onlineEvent f = addEventListener_ "online" f document
