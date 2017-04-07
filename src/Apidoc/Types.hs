{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Apidoc.Types where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import Apidoc.TH
import Apidoc.Internal.Types
--------------------------------------------------------------------------------

apidoc "static/apidoc-spec-service.json"
