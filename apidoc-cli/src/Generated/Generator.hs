{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Generated.Generator where

--------------------------------------------------------------------------------
import Apidoc.Types.Spec (Service)
import Apidoc.TH
--------------------------------------------------------------------------------

apidoc "static/apidoc-generator-service.json"
