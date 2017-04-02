{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Apidoc.Internal.TH where

--------------------------------------------------------------------------------
import Prelude hiding (Enum, read)
import Data.Ratio
import Text.Casing
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Aeson
import qualified Data.Text as T
import Data.Int
import Data.UUID.Types (UUID)
import Data.Attoparsec.Text as A
import Control.Applicative
import Data.Functor
import Debug.Trace
import Data.Map (Map)
import Control.Monad
import GHC.Generics (Generic)
import Data.Typeable
--------------------------------------------------------------------------------
import qualified Apidoc.Types as T
import Apidoc.Internal.TH.Utils
import Apidoc.Internal.TH.Types
--------------------------------------------------------------------------------

read :: FilePath -> Q T.Service
read serviceSpec = do
  json <- runIO (BL.readFile serviceSpec)
  case eitherDecode json of
    Left err  -> error $ "Parser error on " ++ serviceSpec ++ ": " ++ err
    Right api -> return api

modelsFrom :: FilePath -> DecsQ
modelsFrom = fmap (\i -> map renderModel (T._serviceModels i)
                      ++ map renderUnion (T._serviceUnions i)
                      ++ map renderEnum (T._serviceEnums i)
                  ) . read

aesonFrom :: FilePath -> DecsQ
aesonFrom = undefined

renderModel :: T.Model -> Dec
renderModel T.Model{..} = mkData $
  Data (Nm $ T.unpack _modelName) . flip map _modelFields $
    \T.Field{..} -> ( Nm $ T.unpack _fieldName
                    , Ty $ T.unpack _fieldType
                    , _fieldRequired
                    )

renderUnion :: T.Union -> Dec
renderUnion T.Union{..} = mkUnion $
  Union (Nm $ T.unpack _unionName) $
    map (Ty . T.unpack . T._unionTypeType) _unionTypes

renderEnum :: T.Enum -> Dec
renderEnum T.Enum{..} = mkEnum $
  Enum (Nm $ T.unpack _enumName)  $
    map (Nm . T.unpack . T._enumValueName) _enumValues

-- test :: IO ()
-- test = do
--   decs <- runQ $ bootstrap "static/apidoc-spec-service.json"
--   print $ ppr decs
-- 
