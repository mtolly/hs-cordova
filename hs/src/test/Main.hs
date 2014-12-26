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

  runHTMLT body $ "style" $= "font-family: sans-serif;"

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
      action "Update" $ do
        res <- Geo.getCurrentPosition def
        runHTMLT result $ setHTML $ show res
      toggle $ Geo.watchPosition def $ \res ->
        runHTMLT result $ setHTML $ show res

    "h1" </ text "Device Orientation"
    do
      result <- "p" <-/ text "Result here"
      action "Update" $ do
        res <- DO.getCurrentHeading
        runHTMLT result $ setHTML $ show res
      toggle $ DO.watchHeading def $ \res ->
        runHTMLT result $ setHTML $ show res

    "h1" </ text "Device Motion"
    do
      result <- "p" <-/ text "Result here"
      action "Update" $ do
        res <- DM.getCurrentAcceleration
        runHTMLT result $ setHTML $ show res
      toggle $ DM.watchAcceleration def $ \res ->
        runHTMLT result $ setHTML $ show res

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
        runHTMLT result $ setHTML $ show res
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
