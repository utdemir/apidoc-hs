{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module ShouldCompile.Api where

--------------------------------------------------------------------------------
import Apidoc.TH
import Apidoc.Types.Spec (Service)
import ShouldCompile.Generator (Generator, File)
import ShouldCompile.Common (Audit, Reference, Healthcheck)
--------------------------------------------------------------------------------

apidoc "test/static/apidoc-api-service.json"
