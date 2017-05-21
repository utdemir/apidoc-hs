{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant.API
import Servant.Client
import Network.HTTP.Client

import Generated.Api

_ :<|> _ :<|> authenticate :<|> _ :<|> _ = client userApi

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let env  = ClientEnv manager (BaseUrl Http "apidoc-api-elb-989008454.us-east-1.elb.amazonaws.com" 80 "")
  r <- flip runClientM env $ authenticate (Just "me@utdemir.com") (Just "")
  print r
