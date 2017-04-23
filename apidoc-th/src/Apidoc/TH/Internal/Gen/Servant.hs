{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Apidoc.TH.Internal.Gen.Servant where

--------------------------------------------------------------------------------
import Language.Haskell.TH
--------------------------------------------------------------------------------
import Apidoc.TH.Internal.Stage1
import Apidoc.TH.Internal.Gen.Utils()
--------------------------------------------------------------------------------

mkApi :: Resource -> DecsQ
mkApi Resource{..}
  = return []
