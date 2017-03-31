{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Apidoc.Internal.Stage1 where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import Apidoc.Internal.Stage1.TH
--------------------------------------------------------------------------------

stage1 "static/apidoc-spec-service.json"

