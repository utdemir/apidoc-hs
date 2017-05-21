{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Generated.Api where

--------------------------------------------------------------------------------
import Apidoc.TH
import Apidoc.Types.Spec (Service)
import Generated.Generator (Generator, File)
import Generated.Common (Audit, Reference, Healthcheck)
--------------------------------------------------------------------------------

apidoc "static/apidoc-api-service.json"
