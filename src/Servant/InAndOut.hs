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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.InAndOut where

import Data.Aeson
import Data.List (intersperse)
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits
import Servant.API
import Servant

-- =========================
-- InAndOutWithRouteNamesAPI
-- =========================

-- | Each route with a manually provided name will be of type 'InAndOut'.
type InAndOutWithRouteNames a b = (b :: Symbol) :> ReqBody '[JSON] a :> Post '[JSON] a

-- | 'InAndOutAPI' is a collection of POST routes that will receive a JSON
--   object in the Request body, deserialize to Haskell, serialize it from
--   Haskell to JSON and return it in the Response body. Use this to
--   manually define the routes for each type. 'xs' and 'ys' must be the same
--   length or it will not be able to compile properly. Each route must
--   be unique.
type family InAndOutWithRouteNamesAPI (xs :: [*]) (ys :: [Symbol]) where
  InAndOutWithRouteNamesAPI (a ': '[]) (b ': '[]) = InAndOutWithRouteNames a b
  InAndOutWithRouteNamesAPI (a ': as)  (b ': bs)  = (InAndOutWithRouteNames a b) :<|> InAndOutWithRouteNamesAPI as bs


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

-- | 'InAndOutAPI' is a collection of POST routes that will receive a JSON
--   object in the Request body, deserialize to Haskell, serialize it from
--   Haskell to JSON and return it in the Response body. This will automatically
--   create the routes based on the types name. Any type that does not have a
--   predefined instance of 'TypeName' must have an instance of 'Generic'.
type family InAndOutAPI (xs :: [*]) where
  InAndOutAPI (a ': '[]) = InAndOut a
  InAndOutAPI (a ': as) = (InAndOut a) :<|> InAndOutAPI as

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
