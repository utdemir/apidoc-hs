{-# LANGUAGE TemplateHaskell #-}

module Apidoc.Internal.Stage1 where

--------------------------------------------------------------------------------
import Apidoc.Internal.Stage1.TH
--------------------------------------------------------------------------------

$(modelsFor "static/apidoc-spec-service.json")
