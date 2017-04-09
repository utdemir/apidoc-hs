{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Data types used in apidoc @service.json@ files.
--
-- This module is generated using:
--
-- @
-- 'Apidoc.TH.apidocFromURL' "http://www.apidoc.me/bryzek/apidoc-spec/0.11.68/service.json"
-- @
module Apidoc.Types where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import Apidoc.TH
--------------------------------------------------------------------------------

apidoc "static/apidoc-spec-service.json"
