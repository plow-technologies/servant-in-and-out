{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeInType #-}

module Main where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant hiding (HList)
import Servant.API hiding (HList)

import Data.Typeable
import GHC.TypeLits

import Network.Wai
import Network.Wai.Handler.Warp

{-
test :: a -> [a] -> [a]
test sep (a:[]) = [a]
test sep (a:b)  = [a,sep] ++ test sep b
test _   a      = a
-}

data User =
  User
    { name :: Text
    , age  :: Int
    } deriving (Eq,Generic,Show)

instance ToJSON User
instance FromJSON User

data Address =
  Address
    { street  :: Text
    , zipcode :: Text
    } deriving (Eq,Generic,Show)

instance ToJSON Address
instance FromJSON Address

-- type Zed a = (a :: Symbol) :> ReqBody '[JSON] a :> Post '[JSON] a

-- type Zed a = "zed" :> ReqBody '[JSON] a :> Post '[JSON] a

type Zed a b = (b :: Symbol) :> ReqBody '[JSON] a :> Post '[JSON] a
-- zedServer :: Zed Int
-- zedServer = return


type InAndOut a = "zed" :> ReqBody '[JSON] a :> Post '[JSON] a

-- type InAndOut a = (symbolVal a) :> ReqBody '[JSON] a :> Post '[JSON] a

data HList a where
  HNil  :: HList '[]
  HCons :: InAndOut a -> HList as -> HList (InAndOut a ': as)

-- data ABC x where
--  Thing :: ABC x -> ABC (someSymbolVal "asdf")

-- type TestAPI = HList '[User,Address]

-- will break if empty
--  InAndOutAPI '[]        = '[] 

type family InAndOutAPI (xs :: [*]) where
  InAndOutAPI (a ': '[]) = InAndOut a
  InAndOutAPI (a ': as)  = (InAndOut a) :<|> InAndOutAPI as

-- type TestAPI = InAndOutAPI '[User,Address]
type TestAPI = InAndOutAPI '[Int,Text]


{-
server :: Server TestAPI
server = return :<|> return

testAPI :: Proxy TestAPI
testAPI = Proxy

app1 :: Application
app1 = serve testAPI server
-}

type ZedAPI = Zed Int "zed"

server :: Server ZedAPI
server = return

testAPI :: Proxy ZedAPI
testAPI = Proxy

app1 :: Application
app1 = serve testAPI server

-- main = return ()
main = run 8081 app1

-- curl -i -d '123' -H 'Content-type: application/json' -X POST http://localhost:8081/zed
-- curl -i -d 'hello' -H 'Content-type: application/json' -X POST http://localhost:8081/zed
