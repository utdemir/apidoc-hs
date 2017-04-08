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
import Control.Applicative
import Data.Functor
import Debug.Trace
import Data.Map (Map)
import Control.Monad
import GHC.Generics (Generic)
import Data.Typeable
import Network.HTTP
import Network.URI
--------------------------------------------------------------------------------
import qualified Apidoc.Internal.Bootstrap as T
import Apidoc.Internal.TH.Gen
import Apidoc.Internal.TH.Types
--------------------------------------------------------------------------------

parse :: BL.ByteString -> Q T.Service
parse json 
  = case eitherDecode json of
      Left err  -> error $ "Parse error on apidoc spec: " ++ err
      Right api -> return api

read :: FilePath -> Q T.Service
read = runIO . BL.readFile >=> parse

-- TODO: Recursively fetch dependencies
fetch :: String -> Q T.Service
fetch url =
  case parseURI url of
    Nothing -> error $ "Error parsing apidoc uri: " ++ url
    Just uri ->
      runIO (simpleHTTP $ Request uri GET [] "") >>= \case
        Left err   -> error $ "Error fetching apidoc spec: " ++ show err
        Right resp -> case rspCode resp of
          (2, _, _) -> parse . rspBody $ resp
          _ -> error $ "Error fetching apidoc spec: " ++ show resp

-- TODO: Generate deprecation warnings
-- TODO: Generate API definition (probably servant?)
gen :: T.Service -> DecsQ
gen T.Service{..}
  = fmap (concat . concat) . sequence $
      [ forM models $ \m ->
          (++) <$> sequence [ mkData m, mkDataToJSON m, mkDataFromJSON m ] <*> mkDataLens m
      , forM unions $ sequence . flip map [ mkUnion, mkUnionToJSON, mkUnionFromJSON ] . (flip ($))
      , forM enums  $ sequence . flip map [ mkEnum , mkEnumToJSON , mkEnumFromJSON  ] . (flip ($))
      ]
  where
    models = map model' _serviceModels
    unions = map union' _serviceUnions
    enums  = map enum'  _serviceEnums

apidoc :: FilePath -> DecsQ
apidoc = read >=> gen

apidocFromURL :: String -> DecsQ
apidocFromURL = fetch >=> gen
  
model' :: T.Model -> Data
model' T.Model{..}
  = Data (Nm $ T.unpack _modelName) . flip map _modelFields $
      \T.Field{..} -> ( Nm $ T.unpack _fieldName
                      , Ty $ T.unpack _fieldType
                      , _fieldRequired
                      )

union' :: T.Union -> Union
union' T.Union{..}
  = Union (Nm $ T.unpack _unionName) $
      map (Ty . T.unpack . T._unionTypeType) _unionTypes

enum' :: T.Enum -> Enum
enum' T.Enum{..} 
  = Enum (Nm $ T.unpack _enumName) $
      map (Nm . T.unpack . T._enumValueName) _enumValues

