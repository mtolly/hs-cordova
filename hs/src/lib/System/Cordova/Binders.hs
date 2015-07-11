{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Cordova.Binders where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign
import GHC.TypeLits
import System.Cordova.Internal
import Data.Char
import Data.Default
import Data.List
import Data.List.HT (partitionMaybe)
import qualified Data.Text as T
import Control.Arrow (first)

{- |
Creates a binding to an enumeration-like sequence of JS values.
The given declaration should be a normal Haskell Enum-derivable type (null constructors).
The corresponding JS expression is the given 'String' prefix plus the constructor in all caps.
Or, to override the constructor part, add a 'JS' constraint before the constructor.
In the @deriving@ list, in addition to the normal classes, 'ToJSRef' and 'FromJSRef'
are supported, which use those JS expressions.
-}
jsEnum :: String -> Q [Dec] -> Q [Dec]
jsEnum prefix qdecs = do
  let invalid = error "jsEnum: invalid declaration"
  jsClass <- [t| JS |]
  qdecs >>= \case
    [DataD context name vars consts derives] -> do
      -- First, fix the data declaration to remove all the JS stuff.
      -- But also get the list of Haskell/JS mappings, minding the JS constraints.
      let (fixedConsts, constNamesExprs) = unzip $ do
            constr <- consts
            return $ case constr of
              NormalC constName [] -> (constr, (constName, map toUpper $ nameToString constName))
              ForallC [] [AppT klass (LitT (StrTyLit jsExpr))] c@(NormalC constName [])
                | klass == jsClass
                -> (c, (constName, jsExpr))
              _ -> invalid
      -- Now make the binding for each constructor's JS representation.
      jsrefType <- [t| JSRef $(conT name) |]
      (bindings, bindingNames) <- fmap unzip $ forM constNamesExprs $ \(constName, jsExpr) -> do
        bindingName <- newName $ "_" ++ nameToString constName
        let binding = ForeignD $ ImportF JavaScript Unsafe
              (prefix ++ jsExpr) bindingName jsrefType
        return (binding, bindingName)
      -- Now, fix the 'deriving' list by removing ToJSRef and FromJSRef,
      -- but note whether or not they were in there.
      let (fixedDerives, deriveTo, deriveFrom) = let
            (derivesTo  , derives' ) = partition (== ''ToJSRef  ) derives
            (derivesFrom, derives'') = partition (== ''FromJSRef) derives'
            in (derives'', not $ null derivesTo, not $ null derivesFrom)
          fixedDecl = DataD context name vars fixedConsts fixedDerives
          theType = foldl appT (conT name) $ flip map vars $ \case
            PlainTV  varName   -> varT varName
            KindedTV varName _ -> varT varName
      -- Use the bindings to make a ToJSRef instance.
      let lambdaToJSRef = lamCaseE $ flip map (zip constNamesExprs bindingNames) $ \((constName, _), bindingName) ->
            match (conP constName []) (normalB [e| return $(varE bindingName) |]) []
      -- Finally, put all our new declarations together.
      fmap concat $ sequence
        [ return $ [fixedDecl] ++ bindings
        , if deriveTo
          then [d|
            instance ToJSRef $theType where
              toJSRef = $lambdaToJSRef
            |]
          else return []
        , if deriveFrom
          then [d|
            instance FromJSRef $theType where
              fromJSRef = js_fromEnum
            |]
          else return []
        ]
    _ -> invalid

nameToString :: Name -> String
nameToString (Name (OccName s) _) = s

{- |
Imports a JS function with the following features:
* Wraps all arguments and the result in JSRefs, using ToJSRef/FromJSRef
* Automatically makes an interruptible binding if the JS expression uses $c
* Supports a result of "Either e a" via JSEither from the Internal module
-}
jsImport :: Q [Dec] -> Q [Dec]
jsImport qdecs = do
  let invalid = error "jsImport: invalid declaration"
  qdecs >>= \case
    [sig@(SigD name fntype), ValD (VarP name') (NormalB (LitE (StringL jsExpr))) []]
      | name == name'
      -> do
        bindingName <- newName $ "_" ++ nameToString name
        ioType <- [t| IO |]
        let (argTypes, resultType) = let
              go = \case
                AppT (AppT ArrowT t1) t2 -> first (t1 :) $ go t2
                result -> ([], result)
              in go fntype
            arrow t1 t2 = [t| $t1 -> $t2 |]
            jsArgTypes = map (\t -> [t| JSRef $(return t) |]) argTypes
            jsResultType = case resultType of
              AppT io t | io == ioType -> [t| IO (JSRef $(return t)) |]
              _                        -> [t| JSRef $(return resultType) |]
        bindingType <- foldr arrow jsResultType jsArgTypes
        let binding = ForeignD $ ImportF JavaScript bindingMethod
              jsExpr bindingName bindingType
            bindingMethod = if "$c" `isInfixOf` jsExpr then Interruptible else Unsafe
        fmap concat $ sequence
          [ return [sig]
          , fmap (: []) $ valD (varP name) (normalB [e| undefined |]) []
          , return [binding]
          ]
    _ -> invalid

class JS (s :: Symbol) where

doubleAp :: IO (Maybe (a -> b)) -> IO (Maybe a) -> IO (Maybe b)
doubleAp = liftM2 (<*>)

jsRecord :: Q [Dec] -> Q [Dec]
jsRecord qdecs = do
  let invalid = error "jsRecord: invalid declaration"
  jsClass <- [t| JS |]
  qdecs >>= \case
    [DataD context name vars [RecC con fields] derives] -> let
      (fixedDerives, deriveTo, deriveFrom, deriveDefault) = let
        (derivesTo     , derives'  ) = partition (== ''ToJSRef  ) derives
        (derivesFrom   , derives'' ) = partition (== ''FromJSRef) derives'
        (derivesDefault, derives''') = partition (== ''Default  ) derives''
        in (derives''', not $ null derivesTo, not $ null derivesFrom, not $ null derivesDefault)
      theType = foldl appT (conT name) $ flip map vars $ \case
        PlainTV  varName   -> varT varName
        KindedTV varName _ -> varT varName
      newdec = DataD context name vars [RecC con newfields] fixedDerives
      (newfields, fieldNamesExprs) = unzip $ do
        trio@(fieldName, strict, oldType) <- fields
        let unchanged = (trio, (fieldName, nameToString fieldName))
            getJS (AppT klass (LitT (StrTyLit jsExpr))) | klass == jsClass = Just jsExpr
            getJS _                                                        = Nothing
        case oldType of
          ForallT binders constraints t -> case partitionMaybe getJS constraints of
            ([]    , _   ) -> return unchanged
            (js : _, real) -> let
              newtrio = (fieldName, strict, ForallT binders real t)
              in return (newtrio, (fieldName, js))
          _ -> return unchanged
      liftOp :: Q Exp -> Q Exp -> Q Exp -> Q Exp
      liftOp op x y = [e| $op $x $y |]
      in fmap concat $ sequence
        [ return [newdec]
        , if deriveTo
          then [d|
            instance ToJSRef $theType where
              toJSRef rec = do
                obj <- newObj
                $(foldr (liftOp [e| (>>) |]) [e| return obj |] $ do
                  (fname, jsExpr) <- fieldNamesExprs
                  return [e| toJSRef ($(varE fname) rec) >>= \x -> setProp $(litE $ StringL jsExpr) x obj |]
                  )
            |]
          else return []
        , if deriveFrom
          then [d|
            instance FromJSRef $theType where
              fromJSRef obj = $(foldl (liftOp [e| liftM2 (<*>) |]) [e| pure $ pure $(conE con) |] $ do
                (_, jsExpr) <- fieldNamesExprs
                return [e| fromProp (T.pack $(litE $ StringL jsExpr)) obj |]
                )
            |]
          else return []
        , if deriveDefault
          then [d|
            instance Default $theType where
              def = $(foldl appE (conE con) $ map (const [e| def |]) fields)
            |]
          else return []
        ]
    _ -> invalid
