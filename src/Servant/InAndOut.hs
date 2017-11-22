{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.InAndOut where

-- base
import Control.Monad
import Data.List (intersperse)
import GHC.Generics
import GHC.TypeLits

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (Text)

-- aeson
import Data.Aeson

-- servant
import Servant
import Servant.API

-- =========================
-- InAndOutWithRouteNamesAPI
-- =========================

-- | Each route with a manually provided name will be of type 'InAndOut'.
type InAndOutWithRouteNames a b = (b :: Symbol) :> ReqBody '[JSON] a :> Post '[JSON] a

type InAndOutListWithRouteNames a b = (b :: Symbol) :> ReqBody '[JSON] [a] :> Post '[JSON] [a]

-- | 'InAndOutAPI' is a collection of POST routes that will receive a JSON
--   object in the Request body, deserialize to Haskell, serialize it from
--   Haskell to JSON and return it in the Response body. Use this to
--   manually define the routes for each type. 'xs' and 'ys' must be the same
--   length or it will not be able to compile properly. Each route must
--   be unique.
type family InAndOutWithRouteNamesAPI (xs :: [*]) (ys :: [Symbol]) where
  InAndOutWithRouteNamesAPI (a ': '[]) (b ': '[]) = InAndOutWithRouteNames a b
  InAndOutWithRouteNamesAPI (a ': as)  (b ': bs)  = (InAndOutWithRouteNames a b) :<|> InAndOutWithRouteNamesAPI as bs

type family InAndOutListWithRouteNamesAPI (xs :: [*]) (ys :: [Symbol]) where
  InAndOutListWithRouteNamesAPI (a ': '[]) (b ': '[]) = InAndOutListWithRouteNames a b
  InAndOutListWithRouteNamesAPI (a ': as)  (b ': bs)  = (InAndOutListWithRouteNames a b) :<|> InAndOutListWithRouteNamesAPI as bs


-- ===========
-- InAndOutAPI
-- ===========

-- | Convert a the name of a type to a Symbol. If it does not have an instance
--   of Generic than it must be manually entered.
type family TypeName a :: Symbol where
  -- Types which don't have a Generic instance
  TypeName Double = "Double"
  TypeName Int    = "Int"
  TypeName String = "String"
  TypeName Text   = "Text"

  -- Generic instances
  TypeName (M1 D ('MetaData name _ _ _) f ()) = name
  TypeName a = TypeName (Rep a ())

type InAndOut a = (TypeName a) :> ReqBody '[JSON] a :> Post '[JSON] a

type InAndOutList a = (TypeName a) :> ReqBody '[JSON] [a] :> Post '[JSON] [a]


-- | 'InAndOutAPI' is a collection of POST routes that will receive a JSON
--   object in the Request body, deserialize to Haskell, serialize it from
--   Haskell to JSON and return it in the Response body. This will automatically
--   create the routes based on the types name. Any type that does not have a
--   predefined instance of 'TypeName' must have an instance of 'Generic'.
type family InAndOutAPI (xs :: [*]) where
  InAndOutAPI (a ': '[]) = InAndOut a
  InAndOutAPI (a ': as) = (InAndOut a) :<|> InAndOutAPI as


type family InAndOutListAPI (xs :: [*]) where
  InAndOutListAPI (a ': '[]) = InAndOutList a
  InAndOutListAPI (a ': as) = (InAndOutList a) :<|> InAndOutListAPI as



curryN :: Int -> Q Exp
curryN n = do
  f  <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)

genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec
  where mkCurryDec ith = do
          cury <- curryN ith
          let name = mkName $ "curry" ++ show ith
          return $ FunD name [Clause [] (NormalB cury) []]

mkPure :: String -> Q [Dec]
mkPure n = do
  arg <- newName "x"
  let fnName = mkName $ n ++ "Server"
  -- return $ [FunD fnName [Clause [] (NormalB $ LamE [VarP arg] (AppE (VarE (mkName "pure")) (VarE arg)) ) [] ]]
  return $ [FunD fnName [Clause [] (NormalB $ (VarE (mkName "pure")) ) [] ]]

mkServer :: String -> String -> Int -> Q [Dec]
mkServer fn apiName size = do
  let fnName = mkName fn
  let args = foldl (\l r -> UInfixE l (ConE $ mkName ":<|>") r) (VarE $ mkName "pure") (replicate (size-1) (VarE $ mkName "pure"))

  return $
    [ SigD fnName (AppT (ConT $ mkName "Server") (ConT $ mkName apiName))
--    , FunD fnName [Clause [] (NormalB $ (VarE $ mkName "pure") `AppE` (ConE $ mkName "(:<|>)") `AppE` (VarE $ mkName "pure") ) [] ]]
--    , FunD fnName [Clause [] (NormalB $ UInfixE (VarE $ mkName "pure") (ConE $ mkName ":<|>") (VarE $ mkName "pure") ) [] ]]

    , FunD fnName [Clause [] (NormalB args ) [] ]]
{-
UInfixE

type family Returns (lst :: [*]) where
  Returns

class Returns (lst :: [*]) where
  -- returns :: Server (InAndOutAPI lst)
  returns :: Server (InAndOutAPI lst)

instance Returns '[t] where
  returns = return

-- haven't been able to get this to work yet
instance Returns ts => Returns (t ': ts) where
  returns = return :<|> (returns ts)
-}                  
{-
class Returns (lst :: [*]) where
  returns :: Server (InAndOutAPI lst)

instance Returns '[t] where
  returns = return

-- haven't been able to get this to work yet
instance Returns ts => Returns (t ': ts) where
  returns = return :<|> (returns @ts)
-}

{-
-- can easily be done with TH
mkServer :: Int -> Proxy (a :: [*]) -> Server a
mkServer 0 _ = return
mkServer r a = return :<|> (mkServer (r - 1) a)

-- mkServer :: Int -> [Proxy (x :: *)] -> Server (InAndOut2API (xs :: [*]))
-- mkServer rs _ = sequence $ intersperse (:<|>) $ replicate rs mkRoute
-}
