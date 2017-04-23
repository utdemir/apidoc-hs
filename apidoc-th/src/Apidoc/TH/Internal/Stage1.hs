{-# LANGUAGE TemplateHaskell  #-}

module Apidoc.TH.Internal.Stage1 where

--------------------------------------------------------------------------------
import           Apidoc.TH.Internal.Stage1.TH (stage1)
import           Prelude                      hiding (Enum)
--------------------------------------------------------------------------------

stage1 "static/apidoc-spec-service.json"
