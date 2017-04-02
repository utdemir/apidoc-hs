{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Apidoc.Internal.Bootstrap where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import Apidoc.Internal.Bootstrap.TH (bootstrap)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Text.Show.Pretty
--------------------------------------------------------------------------------

bootstrap "static/apidoc-spec-service.json"

-- test :: IO ()
-- test = do
--   contents <- BL.readFile "static/apidoc-spec-service.json"
--   pPrint $ eitherDecode @Service contents
  
