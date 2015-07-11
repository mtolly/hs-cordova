module System.Cordova.Vibration
( vibrate
, vibrateCancel
) where

jsImport "navigator.vibrate($1);" [t| [Int] -> IO () |]
-- no arg form is deprecated.
-- single int arg is equivalent to [int].

vibrateCancel :: IO ()
vibrateCancel = vibrate []
