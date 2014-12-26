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
import qualified System.Cordova.BatteryStatus as Bat
import Data.Default (def)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Data.Function (on)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.List (groupBy)
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (MonadIO, liftIO)

import HTMLT

main :: IO ()
main = do
  waitDeviceReady

  Bar.overlaysWebView False
  Bar.styleBlackOpaque
  Bar.backgroundColorByName "black"

  let toggle :: (MonadIO m) => IO (IO ()) -> HTMLT m ()
      toggle starter = do
        stopper <- liftIO $ newMVar Nothing
        button $ do
          btn <- getElement
          text "Start listening"
          onclick $ takeMVar stopper >>= \case
            Nothing -> do
              newStopper <- starter
              runHTMLT btn $ setHTML "Stop listening"
              putMVar stopper $ Just newStopper
            Just oldStopper -> do
              oldStopper
              runHTMLT btn $ setHTML "Start listening"
              putMVar stopper Nothing

      button :: (MonadIO m) => HTMLT m a -> HTMLT m a
      button act = "button" </ ("type" $= "button") >> act

      textBox :: (MonadIO m) => HTMLT m Element
      textBox = "input" <-/ "type" $= "text"

      action :: (MonadIO m) => String -> IO () -> HTMLT m ()
      action s act = button $ text s >> onclick act

  runHTMLT body $ do

    "style" $= let
      prop (k, v) = k ++ ": " ++ v ++ ";"
      in unlines $ map prop $
        [ ("font-family", "sans-serif")
        , ("background-color", "#115")
        , ("color", "#eee")
        ]

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

    let spaceStatus get watch = do
          result <- "p" <-/ text "Status here"
          let update = runHTMLT result . setHTML . show
          action "Update" $ get >>= update
          toggle $ watch update

    "h1" </ text "Geolocation"
    spaceStatus (Geo.getCurrentPosition def) (Geo.watchPosition def)

    "h1" </ text "Device Orientation"
    spaceStatus DO.getCurrentHeading (DO.watchHeading def)

    "h1" </ text "Device Motion"
    spaceStatus DM.getCurrentAcceleration (DM.watchAcceleration def)

    "h1" </ text "Vibration"
    "form" </ do
      txt <- textBox
      action "Vibrate pattern" $ do
        str <- runHTMLT txt getValue
        let ints = mapMaybe readMaybe $ groupBy ((==) `on` isDigit) str
        Vib.vibrate ints
    action "Vibrate cancel" Vib.vibrateCancel

    "h1" </ text "Network Information"
    do
      result <- "p" <-/ text "Result here"
      action "Update" $ do
        res <- Net.connectionType
        time <- getCurrentTime
        runHTMLT result $ setHTML $ show time ++ ": " ++ show res
    do
      result <- "p" <-/ text "Offline at:"
      toggle $ Net.offlineEvent $ do
        time <- getCurrentTime
        runHTMLT result $ setHTML $ "Offline at: " ++ show time
    do
      result <- "p" <-/ text "Online at:"
      toggle $ Net.onlineEvent $ do
        time <- getCurrentTime
        runHTMLT result $ setHTML $ "Online at: " ++ show time

    "h1" </ text "Status Bar"
    action "Overlay web view" $ Bar.overlaysWebView True
    action "Don't overlay web view" $ Bar.overlaysWebView False
    action "Style: default" Bar.styleDefault
    action "Style: light content" Bar.styleLightContent
    action "Style: black translucent" Bar.styleBlackTranslucent
    action "Style: black opaque" Bar.styleBlackOpaque
    "form" </ do
      txt <- textBox
      button $ do
        text "Set background color by name"
        onclick $ runHTMLT txt getValue >>= Bar.backgroundColorByName
    "form" </ do
      txt <- textBox
      button $ do
        text "Set background color by hex string"
        onclick $ runHTMLT txt getValue >>= Bar.backgroundColorByHexString
    do
      result <- "p" <-/ text "Result here"
      let update = do
            b <- Bar.isVisible
            runHTMLT result $ setHTML $ "Visible: " ++ show b
      action "Hide" $ Bar.hideBar >> update
      action "Show" $ Bar.showBar >> update

    "h1" </ text "Battery"
    let batteryToggle kind batf = do
          result <- "p" <-/ text $ kind ++ " here"
          toggle $ batf $ \status -> do
            time <- getCurrentTime
            runHTMLT result $ setHTML $
              kind ++ " at " ++ show time ++ ": " ++ show status
    batteryToggle "Status" Bat.onStatus
    batteryToggle "Critical status" Bat.onCritical
    batteryToggle "Low status" Bat.onLow
