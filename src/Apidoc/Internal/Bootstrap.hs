{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Apidoc.Internal.Bootstrap where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import Apidoc.Internal.Bootstrap.TH (bootstrap)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
--------------------------------------------------------------------------------

bootstrap "static/apidoc-spec-service.json"

