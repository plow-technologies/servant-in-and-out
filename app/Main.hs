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

import Servant.InAndOut

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

type TestAPI = InAndOutAPI '[Int,Text] ["int","text"]
type TestAPI2 = InAndOutAPI '[User,Address] ["user","address"]
type TestAPI3 = InAndOut2API '[User,Address]

server :: Server TestAPI
server = return :<|> return

testAPI :: Proxy TestAPI
testAPI = Proxy

app1 :: Application
app1 = serve testAPI server


server2 :: Server TestAPI2
server2 = return :<|> return

testAPI2 :: Proxy TestAPI2
testAPI2 = Proxy

app2 :: Application
app2 = serve testAPI2 server2


server3 :: Server TestAPI3
server3 = return :<|> return

testAPI3 :: Proxy TestAPI3
testAPI3 = Proxy

app3 :: Application
app3 = serve testAPI3 server3

main = run 8081 app3

-- server1
-- curl -i -d '123' -H 'Content-type: application/json' -X POST http://localhost:8081/int
-- curl -i -d '"hello"' -H 'Content-type: application/json' -X POST http://localhost:8081/text

-- server2
-- curl -i -d '{"name":"Javier","age":35}' -H 'Content-type: application/json' -X POST http://localhost:8081/user
-- curl -i -d '{"street":"La Casa Blanca","zipcode":"12345"}' -H 'Content-type: application/json' -X POST http://localhost:8081/address

-- server3
-- curl -i -d '{"name":"Javier","age":35}' -H 'Content-type: application/json' -X POST http://localhost:8081/User
-- curl -i -d '{"street":"La Casa Blanca","zipcode":"12345"}' -H 'Content-type: application/json' -X POST http://localhost:8081/Address
