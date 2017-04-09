{-# LANGUAGE TemplateHaskell  #-}

module Apidoc.Internal.Bootstrap where

--------------------------------------------------------------------------------
import           Apidoc.Internal.Bootstrap.TH (bootstrap)
import           Prelude                      hiding (Enum)
--------------------------------------------------------------------------------

bootstrap "static/apidoc-spec-service.json"
