{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module ShouldCompile.Generator where

--------------------------------------------------------------------------------
import Apidoc.Types.Spec (Service)
import Apidoc.TH
--------------------------------------------------------------------------------

apidoc "test/static/apidoc-generator-service.json"
