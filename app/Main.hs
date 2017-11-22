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
{-# LANGUAGE TemplateHaskell #-}

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

type TestAPI  = InAndOutWithRouteNamesAPI '[Int,Text] ["int","text"]
type TestAPI2 = InAndOutWithRouteNamesAPI '[User,Address] ["user","address"]
type TestAPI3 = InAndOutAPI '[User,Address]
type TestAPI4 = InAndOutListWithRouteNamesAPI '[User,Address] ["user","address"]
type TestAPI5 = InAndOutListAPI '[User,Address,Text]

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

server4 :: Server TestAPI4
server4 = return :<|> return

testAPI4 :: Proxy TestAPI4
testAPI4 = Proxy

app4 :: Application
app4 = serve testAPI4 server4

--server5 :: Server TestAPI5
--server5 = return :<|> return
$(mkServer "server5" "TestAPI5" 3)

testAPI5 :: Proxy TestAPI5
testAPI5 = Proxy

app5 :: Application
app5 = serve testAPI5 server5

$(genCurries 20)
$(mkPure "thing")
{- curry1 -}

main = do
  r <- thingServer 1
  print r
  run 8081 app5

-- server1
-- curl -i -d '123' -H 'Content-type: application/json' -X POST http://localhost:8081/int
-- curl -i -d '"hello"' -H 'Content-type: application/json' -X POST http://localhost:8081/text

-- server2
-- curl -i -d '{"name":"Javier","age":35}' -H 'Content-type: application/json' -X POST http://localhost:8081/user
-- curl -i -d '{"street":"La Casa Blanca","zipcode":"12345"}' -H 'Content-type: application/json' -X POST http://localhost:8081/address

-- server3
-- curl -i -d '{"name":"Javier","age":35}' -H 'Content-type: application/json' -X POST http://localhost:8081/User
-- curl -i -d '{"street":"La Casa Blanca","zipcode":"12345"}' -H 'Content-type: application/json' -X POST http://localhost:8081/Address

-- server4
-- curl -i -d '[{"name":"Javier","age":35},{"name":"Antonio","age":43}]' -H 'Content-type: application/json' -X POST http://localhost:8081/user
-- curl -i -d '[{"street":"La Casa Blanca","zipcode":"12345"},{"street":"Palacio de la Moncloa","zipcode":"33333"}]' -H 'Content-type: application/json' -X POST http://localhost:8081/address

-- server5
-- curl -i -d '[{"name":"Javier","age":35}]' -H 'Content-type: application/json' -X POST http://localhost:8081/User
-- curl -i -d '[{"street":"La Casa Blanca","zipcode":"12345"}]' -H 'Content-type: application/json' -X POST http://localhost:8081/Address
