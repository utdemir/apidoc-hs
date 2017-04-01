module Apidoc.Internal.TH.Types where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import Language.Haskell.TH
--------------------------------------------------------------------------------

newtype Ty = Ty String
newtype Nm = Nm String
data Data  = Data Nm [(Nm, Ty, Bool)]
data Enum  = Enum Nm [Nm]
data Union = Union Nm [Ty]


