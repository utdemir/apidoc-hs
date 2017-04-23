{-# LANGUAGE TemplateHaskell #-}

module Apidoc.Types.Generator where

--------------------------------------------------------------------------------
import Apidoc.Types.Spec (Service)
import Apidoc.TH
--------------------------------------------------------------------------------

apidoc "static/apidoc-generator-service.json"
