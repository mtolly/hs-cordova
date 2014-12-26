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
import GHCJS.Foreign
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Applicative ((<$>))
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

  setAttribute "style" "font-family: sans-serif;" body

  let toggle :: (MonadIO m) => IO (IO ()) -> HTMLT m ()
      toggle starter = do
        stopper <- liftIO $ newMVar Nothing
        button $ do
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
        setHTML (toJSString $ show res) result
      toggle $ Geo.watchPosition def $ \res ->
        setHTML (toJSString $ show res) result

    "h1" </ text "Device Orientation"
    do
      result <- "p" <-/ text "Result here"
      action "Update" $ do
        res <- DO.getCurrentHeading
        setHTML (toJSString $ show res) result
      toggle $ DO.watchHeading def $ \res ->
        setHTML (toJSString $ show res) result

    "h1" </ text "Device Motion"
    do
      result <- "p" <-/ text "Result here"
      action "Update" $ do
        res <- DM.getCurrentAcceleration
        setHTML (toJSString $ show res) result
      toggle $ DM.watchAcceleration def $ \res ->
        setHTML (toJSString $ show res) result

    "h1" </ text "Vibration"
    "form" </ do
      txt <- textBox
      action "Vibrate pattern" $ do
        str <- fromJSString <$> getValue txt
        let ints = mapMaybe readMaybe $ groupBy ((==) `on` isDigit) str
        Vib.vibrate ints
    action "Vibrate cancel" Vib.vibrateCancel

    "h1" </ text "Network Information"
    do
      result <- "p" <-/ text "Result here"
      action "Update" $ do
        res <- Net.connectionType
        setHTML (toJSString $ show res) result
    do
      result <- "p" <-/ text "Offline at:"
      toggle $ Net.offlineEvent $ do
        time <- getCurrentTime
        setHTML (toJSString $ "Offline at: " ++ show time) result
    do
      result <- "p" <-/ text "Online at:"
      toggle $ Net.onlineEvent $ do
        time <- getCurrentTime
        setHTML (toJSString $ "Online at: " ++ show time) result

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
        onclick $ getValue txt >>= Bar.backgroundColorByName . fromJSString
    "form" </ do
      txt <- textBox
      button $ do
        text "Set background color by hex string"
        onclick $ getValue txt >>= Bar.backgroundColorByHexString . fromJSString
    do
      result <- "p" <-/ text "Result here"
      let update = do
            b <- Bar.isVisible
            setHTML (toJSString $ "Visible: " ++ show b) result
      action "Hide" $ Bar.hideBar >> update
      action "Show" $ Bar.showBar >> update
