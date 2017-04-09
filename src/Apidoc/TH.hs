-- |
-- Use the functions below to generate datatypes for an apidoc @service.json@
-- file.
--
-- Currently models, enums and unions are supported.
--
-- The datatypes will have @Show@, @Eq@ and @Typeable@, @ToJSON@ and @FromJSON@
-- instances by default. They will also have @Generic@ instance if @DeriveGeneric@
-- extension is enabled either globally or via using a LANGUAGE pragma.
--
-- This library does *not* resolve imports. The user is expected to call below
-- functions for every required file manually.
--
-- In case of name clashes, you will get a compile time error. You can solve most
-- of them by separating them to different modules and hiding the problematic
-- names on imports.
--
-- See 'Apidoc.Types' module as an example for the generated code.
--
module Apidoc.TH where

--------------------------------------------------------------------------------
import qualified Apidoc.Internal.TH  as T
import           Language.Haskell.TH (DecsQ)
--------------------------------------------------------------------------------

-- |
-- Generates data types for given apidoc schema from a local file.
--
-- Accepts absolute or relative paths.
--
-- @
-- {-\# LANGUAGE TemplateHaskell, DeriveGeneric \#-}
--
-- module MyTypes where
--
-- apidoc "static/service.json"
-- @
apidoc :: FilePath -> DecsQ
apidoc = T.apidoc

-- |
-- Generates data types for given apidoc schema from an URL.
--
-- Only supports HTTP.
--
-- @
-- {-\# LANGUAGE TemplateHaskell, DeriveGeneric \#-}
--
-- module MyTypes where
--
-- apidocFromURL "http://www.apidoc.me/bryzek/apidoc-spec/0.11.68/service.json"
-- @
apidocFromURL :: String -> DecsQ
apidocFromURL = T.apidocFromURL
