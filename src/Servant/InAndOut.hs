{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.InAndOut where

import Data.Aeson
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
