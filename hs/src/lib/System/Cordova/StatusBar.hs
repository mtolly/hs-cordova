{-# LANGUAGE JavaScriptFFI #-}
module System.Cordova.StatusBar
( overlaysWebView
, styleDefault, styleLightContent, styleBlackTranslucent, styleBlackOpaque
, backgroundColorByName, backgroundColorByHexString
, hideBar, showBar, isVisible
) where

import qualified Data.Text as T

jsImport "StatusBar.overlaysWebView($1);" [t| Bool -> IO () |]

jsImport "StatusBar.styleDefault"          [t| IO () |]
jsImport "StatusBar.styleLightContent"     [t| IO () |]
jsImport "StatusBar.styleBlackTranslucent" [t| IO () |]
jsImport "StatusBar.styleBlackOpaque"      [t| IO () |]

jsImport "StatusBar.backgroundColorByName     ($1);" [t| T.Text -> IO () |]
jsImport "StatusBar.backgroundColorByHexString($1);" [t| T.Text -> IO () |]

jsImport "StatusBar.hide();" [d| hideBar :: IO () |]
jsImport "StatusBar.show();" [d| showBar :: IO () |]
jsImport "StatusBar.isVisible" [t| IO Bool |]
