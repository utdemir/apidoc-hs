{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client

import Generated.Api

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  undefined
