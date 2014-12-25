{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Cordova.Base
import qualified System.Cordova.StatusBar as Bar
import qualified System.Cordova.Device as Dev
import qualified System.Cordova.Geolocation as Geo
import qualified System.Cordova.DeviceOrientation as DO
import qualified System.Cordova.DeviceMotion as DM
import qualified System.Cordova.Vibration as Vib
import qualified System.Cordova.NetworkInformation as Net
import Data.Default
import GHCJS.Foreign
import Control.Concurrent.MVar
import Control.Applicative ((<$>))
import Data.Function (on)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.List (groupBy)
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class

import HTMLT

main :: IO ()
main = do
  waitDeviceReady

  Bar.overlaysWebView False
  Bar.styleBlackOpaque
  Bar.backgroundColorByName "black"

  setAttribute "style" "font-family: sans-serif;" body

  let newToggle :: (MonadIO m) => IO (IO ()) -> HTMLT m ()
      newToggle starter = do
        stopper <- liftIO $ newMVar Nothing
        "button" </ do
          btn <- getElement
          text "Start listening"
          onclick $ takeMVar stopper >>= \case
            Nothing -> do
              newStopper <- starter
              setHTML "Stop listening" btn
              putMVar stopper $ Just newStopper
            Just oldStopper -> do
              oldStopper
              setHTML "Start listening" btn
              putMVar stopper Nothing

  runHTMLT body $ do

    "h1" </ text "Device"
    "table" </ do
      let row (k, v) = "tr" </ do
            "td" </ text k
            "td" </ text v
      row ("cordova" , show Dev.cordova )
      row ("model"   , show Dev.model   )
      row ("platform", show Dev.platform)
      row ("uuid"    , show Dev.uuid    )
      row ("version" , show Dev.version )

    "h1" </ text "Geolocation"
    do
      result <- "p" <-/ text "Result here"
      "button" </ do
        text "Update"
        onclick $ do
          res <- Geo.getCurrentPosition def
          setHTML (toJSString $ show res) result
      newToggle $ Geo.watchPosition def $ \res ->
        setHTML (toJSString $ show res) result

    "h1" </ text "Device Orientation"
    do
      result <- "p" <-/ text "Result here"
      "button" </ do
        text "Update"
        onclick $ do
          res <- DO.getCurrentHeading
          setHTML (toJSString $ show res) result
      newToggle $ DO.watchHeading def $ \res ->
        setHTML (toJSString $ show res) result

    "h1" </ text "Device Motion"
    do
      result <- "p" <-/ text "Result here"
      "button" </ do
        text "Update"
        onclick $ do
          res <- DM.getCurrentAcceleration
          setHTML (toJSString $ show res) result
      newToggle $ DM.watchAcceleration def $ \res ->
        setHTML (toJSString $ show res) result

    "h1" </ text "Vibration"
    "form" </ do
      txt <- "input" <-/ do
        "type" $= "text"
      "button" </ do
        text "Vibrate pattern"
        "type" $= "button"
        onclick $ do
          str <- fromJSString <$> getValue txt
          let ints = mapMaybe readMaybe $ groupBy ((==) `on` isDigit) str
          Vib.vibrate ints
    "button" </ do
      text "Vibrate cancel"
      onclick Vib.vibrateCancel

    "h1" </ text "Network Information"
    do
      result <- "p" <-/ text "Result here"
      "button" </ do
        text "Update"
        onclick $ do
          res <- Net.connectionType
          setHTML (toJSString $ show res) result
    do
      result <- "p" <-/ text "Offline at:"
      newToggle $ Net.offlineEvent $ do
        time <- getCurrentTime
        setHTML (toJSString $ "Offline at: " ++ show time) result
    do
      result <- "p" <-/ text "Online at:"
      newToggle $ Net.onlineEvent $ do
        time <- getCurrentTime
        setHTML (toJSString $ "Online at: " ++ show time) result
