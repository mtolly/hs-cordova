
module System.Cordova.Camera.PopoverOptions where
import GHCJS.Foreign
import GHCJS.Marshal
import Data.Default
import Control.Applicative
import System.Cordova.Internal
data PopoverOptions = PopoverOptions { popX :: Maybe Int, popY :: Maybe Int, popWidth :: Maybe Int, popHeight :: Maybe Int, arrowDir :: Maybe Int } deriving (Eq, Ord, Show, Read)
instance Default PopoverOptions where def = PopoverOptions def def def def def
instance ToJSRef PopoverOptions where
  toJSRef opts = do
    obj <- newObj
    let _setJust s f = case f opts of
          Nothing -> return ()
          Just x -> toJSRef x >>= \ref -> setProp s ref obj
        _set s f = toJSRef (f opts) >>= \ref -> setProp s ref obj
    _setJust "x" popX
    _setJust "y" popY
    _setJust "width" popWidth
    _setJust "height" popHeight
    _setJust "PopoverArrowDirection" arrowDir
    return obj
instance FromJSRef PopoverOptions where
  fromJSRef obj = do
    _x0 <- fromProp "x" obj
    _x1 <- fromProp "y" obj
    _x2 <- fromProp "width" obj
    _x3 <- fromProp "height" obj
    _x4 <- fromProp "PopoverArrowDirection" obj
    return $ PopoverOptions <$> _x0 <*> _x1 <*> _x2 <*> _x3 <*> _x4
