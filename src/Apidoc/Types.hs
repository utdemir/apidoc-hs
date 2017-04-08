{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Apidoc.Types where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import Apidoc.TH
--------------------------------------------------------------------------------

apidoc "static/apidoc-spec-service.json"
