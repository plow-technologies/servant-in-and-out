{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.InAndOut where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits
import Servant.API

{-
data HList a where
  HNil  :: HList '[]
  HCons :: InAndOut a b -> HList as -> HList (InAndOut a b ': as)
-}

-- will break if empty
-- xs and ys must be the same length
type family InAndOutAPI (xs :: [*]) (ys :: [Symbol]) where
  InAndOutAPI (a ': '[]) (b ': '[]) = InAndOut a b
  InAndOutAPI (a ': as)  (b ': bs)  = (InAndOut a b) :<|> InAndOutAPI as bs

type InAndOut a b = (b :: Symbol) :> ReqBody '[JSON] a :> Post '[JSON] a

-- | Convert a the name of a type to a Symbol. If it does not have an instance
--   of Generic than it must be manually entered.
type family TypeName a :: Symbol where
  -- Types which don't have a Generic instance
  TypeName Int    = "Int"
  TypeName Text   = "Text"
  TypeName String = "String"

  -- Generic instances
  TypeName (M1 D ('MetaData name _ _ _) f ()) = name
  TypeName a = TypeName (Rep a ())

type InAndOut2 a = (TypeName a) :> ReqBody '[JSON] a :> Post '[JSON] a

type family InAndOut2API (xs :: [*]) where
  InAndOut2API (a ': '[]) = InAndOut2 a
  InAndOut2API (a ': as) = (InAndOut2 a) :<|> InAndOut2API as
