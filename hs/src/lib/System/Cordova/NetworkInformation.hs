{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Cordova.NetworkInformation
( Connection(..)
, connectionType
, offlineEvent
, onlineEvent
) where

import System.Cordova.EventListener
import System.Cordova.Binders
import qualified Data.Text as T
import GHCJS.Marshal

jsEnum "Connection." [d|
  data Connection
    = Unknown
    | Ethernet
    | Wifi
    | JS "CELL_2G" => Cell2G
    | JS "CELL_3G" => Cell3G
    | JS "CELL_4G" => Cell4G
    | Cell
    | None
    deriving (Eq, Ord, Show, Read, Enum, Bounded, ToJSRef, FromJSRef)
  |]

jsImport [d|
  connectionType :: IO Connection
  connectionType = jsCode "navigator.connection.type"
  |]

offlineEvent :: IO () -> IO (IO ())
offlineEvent f = addEventListener (T.pack "offline") f document

onlineEvent :: IO () -> IO (IO ())
onlineEvent f = addEventListener (T.pack "online") f document
