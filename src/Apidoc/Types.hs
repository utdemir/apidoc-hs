{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Apidoc.Types where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import Apidoc.TH
--------------------------------------------------------------------------------

-- apidocFromURL http://www.apidoc.me/bryzek/apidoc-generator/0.11.68/service.json
apidoc "static/apidoc-spec-service.json"
