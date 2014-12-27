{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
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
import qualified System.Cordova.Camera as Cam
import qualified System.Cordova.Dialogs as Dia
import qualified System.Cordova.Globalization as Glo
import Data.Default (def)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Data.Function (on)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.List (groupBy)
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_, liftM2, liftM3, liftM4, join)

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

      textBox :: (MonadIO m) => String -> HTMLT m Element
      textBox s = "input" <-/ do
        "type" $= "text"
        "value" $= s

      action :: (MonadIO m) => String -> IO () -> HTMLT m ()
      action s act = button $ text s >> onclick act

  runHTMLT headElement $ do
    style "body"
      [ ("font-family", "sans-serif")
      , ("background-color", "#115")
      , ("color", "#eee")
      ]
    style "form"
      [ ("border", "1px solid #666")
      ]
    style "td"
      [ ("border", "1px solid #666")
      ]

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

    let spaceStatus get watch = "form" </ do
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
      txt <- textBox "500 100 200"
      action "Vibrate pattern" $ do
        str <- runHTMLT txt getValue
        let ints = mapMaybe readMaybe $ groupBy ((==) `on` isDigit) str
        Vib.vibrate ints
    action "Vibrate cancel" Vib.vibrateCancel

    "h1" </ text "Network Information"
    "form" </ do
      result <- "p" <-/ text "Result here"
      action "Update" $ do
        res <- Net.connectionType
        time <- getCurrentTime
        runHTMLT result $ setHTML $ show time ++ ": " ++ show res
    "form" </ do
      result <- "p" <-/ text "Offline at:"
      toggle $ Net.offlineEvent $ do
        time <- getCurrentTime
        runHTMLT result $ setHTML $ "Offline at: " ++ show time
    "form" </ do
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
      txt <- textBox "red"
      button $ do
        text "Set background color by name"
        onclick $ runHTMLT txt getValue >>= Bar.backgroundColorByName
    "form" </ do
      txt <- textBox "#ff00ff"
      button $ do
        text "Set background color by hex string"
        onclick $ runHTMLT txt getValue >>= Bar.backgroundColorByHexString
    "form" </ do
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

    "h1" </ text "Camera"
    "form" </ do
      stamp <- "p" <-/ text "Time here"
      img <- "img" <-/ "width" $= "300"
      err <- "p" <-/ text "Error here"
      let combos :: [(Cam.SourceType, Cam.DestinationType)]
          combos = liftM2 (,) [minBound .. maxBound] [minBound .. maxBound]
      forM_ combos $ \(stype, dtype) -> do
        action (show stype ++ " &rarr; " ++ show dtype) $ do
          let opts = def
                { Cam.sourceType = Just stype
                , Cam.destinationType = Just dtype
                }
          pic <- Cam.getPicture opts
          time <- getCurrentTime
          case pic of
            Left e -> runHTMLT err $ setHTML $ show e ++ " at " ++ show time
            Right url -> do
              runHTMLT img $ "src" $= url
              runHTMLT stamp $ setHTML $ show time
    "form" </ do
      result <- "p" <-/ text "Result here"
      action "Cleanup" $ do
        res <- Cam.cleanup
        time <- getCurrentTime
        runHTMLT result $ setHTML $ show res ++ " at " ++ show time

    "h1" </ text "Dialogs"
    buttonPushed <- "p" <-/ text "Button here"
    let val :: (Read a) => Element -> IO a
        val elt = fmap read $ runHTMLT elt getValue
        push :: (Show a) => a -> IO ()
        push i = do
          time <- getCurrentTime
          runHTMLT buttonPushed $ setHTML $ show i ++ " at " ++ show time
    "form" </ do
      t1 <- textBox $ show "The message"
      t2 <- textBox $ show $ Just "The title"
      t3 <- textBox $ show $ Just "The button"
      action "Alert" $ join (liftM3 Dia.alert (val t1) (val t2) (val t3)) >>= push
    "form" </ do
      t1 <- textBox $ show "The message"
      t2 <- textBox $ show $ Just "The title"
      t3 <- textBox $ show $ Just ["B1", "B2", "B3"]
      action "Confirm" $ join (liftM3 Dia.confirm (val t1) (val t2) (val t3)) >>= push
    "form" </ do
      t1 <- textBox $ show "The message"
      t2 <- textBox $ show $ Just "The title"
      t3 <- textBox $ show $ Just ["B1", "B2", "B3"]
      t4 <- textBox $ show $ Just "Input"
      action "Prompt" $ join (liftM4 Dia.prompt (val t1) (val t2) (val t3) (val t4)) >>= push
    "form" </ do
      t1 <- textBox "2"
      action "Beep" $ val t1 >>= Dia.beep

    "h1" </ text "Globalization"
    "table" </ do
      let row (k, act) = "tr" </ do
            "td" </ text k
            res <- liftIO act
            "td" </ text $ show res
      row ("getPreferredLanguage", Glo.getPreferredLanguage)
      row ("getLocaleName"       , Glo.getLocaleName       )
      row ("getFirstDayOfWeek"   , Glo.getFirstDayOfWeek   )
    now <- liftIO getCurrentTime
    "form" </ mdo
      t <- textBox $ show now
      action "isDayLightSavingsTime" $ do
        res <- val t >>= Glo.isDayLightSavingsTime
        runHTMLT result $ setHTML $ show res
      result <- "p" <-/ text "Result here"
      return ()
