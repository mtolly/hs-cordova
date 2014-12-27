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
import Data.Maybe (fromJust)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forM_, liftM, liftM2, liftM3, liftM4, join)

import HTMLT

foreign import javascript unsafe
  "$1.valueAsNumber"
  valueAsNumber :: Element -> IO Double

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

  let enterStr :: (MonadIO m) => String -> HTMLT m (m String)
      enterStr s = do
        elt <- textBox s
        return $ runHTMLT elt getValue

      enterDateTime :: (MonadIO m) => HTMLT m (m UTCTime)
      enterDateTime = do
        d <- "input" <-/ "type" $= "date"
        t <- "input" <-/ "type" $= "time"
        return $ do
          milli <- liftIO $ liftM2 (+) (valueAsNumber d) (valueAsNumber t)
          let posix = realToFrac $ milli / 1000
          return $ posixSecondsToUTCTime posix

      enterInt :: (MonadIO m, Integral a) => a -> HTMLT m (m a)
      enterInt n = "input" </ do
        "type" $= "number"
        "value" $= show $ toInteger n
        elt <- getElement
        return $ do
          x <- liftM read $ runHTMLT elt getValue
          return $ fromInteger x

      enterFrac :: (MonadIO m, RealFrac a) => a -> HTMLT m (m a)
      enterFrac n = "input" </ do
        "type" $= "number"
        "value" $= show (realToFrac n :: Double)
        elt <- getElement
        return $ do
          x <- liftM read $ runHTMLT elt getValue
          return $ realToFrac (x :: Double)

      enterRead :: (MonadIO m, Show a, Read a) => a -> HTMLT m (m a)
      enterRead x = do
        f <- enterStr $ show x
        return $ liftM read f

      enterEnum :: (MonadIO m, Enum a, Bounded a, Show a) => HTMLT m (m a)
      enterEnum = "select" </ do
        let enumTable = zip ([0..] :: [Int]) [minBound .. maxBound]
        forM_ enumTable $ \(i, val) -> "option" </ do
          "value" $= show i
          text $ show val
        sel <- getElement
        return $ do
          str <- runHTMLT sel getValue
          return $ fromJust $ lookup (read str) enumTable

      enterMaybe :: (MonadIO m) =>
        HTMLT m (m a) -> HTMLT m (m (Maybe a))
      enterMaybe input = "p" </ do
        box <- "input" <-/ do
          "type" $= "checkbox"
          setChecked True
        get <- input
        return $ do
          chk <- runHTMLT box checked
          if chk
            then liftM Just get
            else return Nothing

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

    let spaceStatus get watch = "form" </ mdo
          let update = runHTMLT result . setHTML . show
          action "Update" $ get >>= update
          toggle $ watch update
          result <- "p" <-/ text "Status here"
          return ()

    "h1" </ text "Geolocation"
    spaceStatus (Geo.getCurrentPosition def) (Geo.watchPosition def)

    "h1" </ text "Device Orientation"
    spaceStatus DO.getCurrentHeading (DO.watchHeading def)

    "h1" </ text "Device Motion"
    spaceStatus DM.getCurrentAcceleration (DM.watchAcceleration def)

    "h1" </ text "Vibration"
    "form" </ do
      t <- enterRead [500, 100, 200]
      action "Vibrate pattern" $ t >>= Vib.vibrate
    action "Vibrate cancel" Vib.vibrateCancel

    "h1" </ text "Network Information"
    "form" </ mdo
      action "Update" $ do
        res <- Net.connectionType
        time <- getCurrentTime
        runHTMLT result $ setHTML $ show time ++ ": " ++ show res
      result <- "p" <-/ text "Result here"
      return ()
    "form" </ mdo
      toggle $ Net.offlineEvent $ do
        time <- getCurrentTime
        runHTMLT result $ setHTML $ "Offline at: " ++ show time
      result <- "p" <-/ text "Offline at:"
      return ()
    "form" </ mdo
      toggle $ Net.onlineEvent $ do
        time <- getCurrentTime
        runHTMLT result $ setHTML $ "Online at: " ++ show time
      result <- "p" <-/ text "Online at:"
      return ()

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
    "form" </ mdo
      let update = do
            b <- Bar.isVisible
            runHTMLT result $ setHTML $ "Visible: " ++ show b
      action "Hide" $ Bar.hideBar >> update
      action "Show" $ Bar.showBar >> update
      result <- "p" <-/ text "Result here"
      return ()

    "h1" </ text "Battery"
    let batteryToggle kind batf = "form" </ mdo
          toggle $ batf $ \status -> do
            time <- getCurrentTime
            runHTMLT result $ setHTML $
              kind ++ " at " ++ show time ++ ": " ++ show status
          result <- "p" <-/ text $ kind ++ " here"
          return ()
    batteryToggle "Status" Bat.onStatus
    batteryToggle "Critical status" Bat.onCritical
    batteryToggle "Low status" Bat.onLow

    "h1" </ text "Camera"
    "form" </ mdo
      stype <- enterEnum
      dtype <- enterEnum
      action "getPicture" $ do
        st <- stype
        dt <- dtype
        let opts = def
              { Cam.sourceType      = Just st
              , Cam.destinationType = Just dt
              }
        pic <- Cam.getPicture opts
        time <- getCurrentTime
        case pic of
          Left e -> runHTMLT err $ setHTML $ show e ++ " at " ++ show time
          Right url -> do
            runHTMLT img $ "src" $= url
            runHTMLT stamp $ setHTML $ show time
      stamp <- "p" <-/ text "Time here"
      img <- "img" <-/ "width" $= "300"
      err <- "p" <-/ text "Error here"
      return ()
    "form" </ mdo
      action "Cleanup" $ do
        res <- Cam.cleanup
        time <- getCurrentTime
        runHTMLT result $ setHTML $ show res ++ " at " ++ show time
      result <- "p" <-/ text "Result here"
      return ()

    "h1" </ text "Dialogs"
    buttonPushed <- "p" <-/ text "Button here"
    let push :: (Show a) => a -> IO ()
        push i = do
          time <- getCurrentTime
          runHTMLT buttonPushed $ setHTML $ show i ++ " at " ++ show time
    "form" </ do
      t1 <- enterStr "The message"
      t2 <- enterMaybe $ enterStr "The title"
      t3 <- enterMaybe $ enterStr "The button"
      action "Alert" $ join (liftM3 Dia.alert t1 t2 t3) >>= push
    "form" </ do
      t1 <- enterStr "The message"
      t2 <- enterMaybe $ enterStr "The title"
      t3 <- enterMaybe $ enterRead ["B1", "B2", "B3"]
      action "Confirm" $ join (liftM3 Dia.confirm t1 t2 t3) >>= push
    "form" </ do
      t1 <- enterStr "The message"
      t2 <- enterMaybe $ enterStr "The title"
      t3 <- enterMaybe $ enterRead ["B1", "B2", "B3"]
      t4 <- enterMaybe $ enterStr "Input"
      action "Prompt" $ join (liftM4 Dia.prompt t1 t2 t3 t4) >>= push
    "form" </ do
      t1 <- enterInt 2
      action "Beep" $ t1 >>= Dia.beep

    "h1" </ text "Globalization"
    "table" </ do
      let row (k, act) = "tr" </ do
            "td" </ text k
            res <- liftIO act
            "td" </ text $ show res
      row ("getPreferredLanguage", Glo.getPreferredLanguage)
      row ("getLocaleName"       , Glo.getLocaleName       )
      row ("getFirstDayOfWeek"   , Glo.getFirstDayOfWeek   )
    "form" </ mdo
      t <- enterDateTime
      action "isDayLightSavingsTime" $ do
        res <- t >>= Glo.isDayLightSavingsTime
        runHTMLT result $ setHTML $ show res
      result <- "p" <-/ text "Result here"
      return ()
    "form" </ mdo
      t1 <- enterStr "123.45"
      t2 <- enterEnum
      action "stringToNumber" $ do
        res <- join $ liftM2 Glo.stringToNumber t1 $ liftM (Glo.NumStrOptions . Just) t2
        runHTMLT result $ setHTML $ show res
      result <- "p" <-/ text "Result here"
      return ()
    "form" </ mdo
      t1 <- enterFrac 12345.6789
      t2 <- enterEnum
      action "numberToString" $ do
        res <- join $ liftM2 Glo.numberToString t1 $ liftM (Glo.NumStrOptions . Just) t2
        runHTMLT result $ setHTML $ show res
      result <- "p" <-/ text "Result here"
      return ()
    "form" </ mdo
      t <- enterDateTime
      action "dateToString" $ do
        res <- t >>= \v -> Glo.dateToString v def
        runHTMLT result $ setHTML $ show res
      result <- "p" <-/ text "Result here"
      return ()
    "form" </ mdo
      t <- enterStr "12/26/14, 9:09 PM"
      action "stringToDate" $ do
        res <- t >>= \v -> Glo.stringToDate v def
        runHTMLT result $ setHTML $ show res
      result <- "p" <-/ text "Result here"
      return ()
