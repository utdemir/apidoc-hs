{-# LANGUAGE DeriveGeneric #-}

module Apidoc.Internal.Types where

--------------------------------------------------------------------------------
import GHC.Generics
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import Data.Time.Clock
import Data.Aeson
import Data.Time.Calendar
import qualified Data.Text as T
--------------------------------------------------------------------------------

instance FromJSON UUID where
  parseJSON j = parseJSON j >>= \t ->
    maybe (fail $ "Invalid UUID: " ++ T.unpack t) return (UUID.fromText t)

instance ToJSON UUID where
  toJSON = toJSON . UUID.toText

