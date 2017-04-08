{-# OPTIONS_GHC -fno-warn-orphans #-}

module Apidoc.Internal.Types where

--------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Text       as T
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
--------------------------------------------------------------------------------

instance FromJSON UUID where
  parseJSON j = parseJSON j >>= \t ->
    maybe (fail $ "Invalid UUID: " ++ T.unpack t) return (UUID.fromText t)

instance ToJSON UUID where
  toJSON = toJSON . UUID.toText

