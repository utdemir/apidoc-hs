{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Apidoc.Internal.Stage1 where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import Apidoc.Internal.Stage1.TH (stage1)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Text.Show.Pretty
--------------------------------------------------------------------------------

stage1 "static/apidoc-spec-service.json"

test :: IO ()
test = do
  contents <- BL.readFile "static/apidoc-spec-service.json"
  pPrint $ eitherDecode @Service contents
  
