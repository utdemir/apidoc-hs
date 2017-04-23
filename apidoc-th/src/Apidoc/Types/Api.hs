{-# LANGUAGE TemplateHaskell #-}

module Apidoc.Types.Api where

--------------------------------------------------------------------------------
import Apidoc.TH
import Apidoc.Types.Spec (Service)
import Apidoc.Types.Generator (Generator, File)
import Apidoc.Types.Common (Audit, Reference)
--------------------------------------------------------------------------------

apidoc "static/apidoc-api-service.json"
