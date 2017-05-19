{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Apidoc.TH.Internal.Stage2 where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Aeson                (eitherDecode)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import           Language.Haskell.TH
import           Network.HTTP
import           Network.URI (parseURI)
import           Prelude                   hiding (Enum, read)
import Language.Haskell.TH.Syntax
--------------------------------------------------------------------------------
import qualified Apidoc.TH.Internal.Stage1 as T
import           Apidoc.TH.Internal.Gen.Simple
import           Apidoc.TH.Internal.Gen.Servant
--------------------------------------------------------------------------------

apidoc :: FilePath -> DecsQ
apidoc = read >=> gen

apidocFromURL :: String -> DecsQ
apidocFromURL = fetch >=> gen

parse :: BL.ByteString -> Q T.Service
parse json = case eitherDecode json of
      Left err  -> error $ "Parse error on apidoc spec: " ++ err
      Right api -> return api

read :: FilePath -> Q T.Service
read path = addDependentFile path >> runIO (BL.readFile path) >>= parse

fetch :: String -> Q T.Service
fetch url =
  case parseURI url of
    Nothing -> error $ "Error parsing apidoc uri: " ++ url
    Just uri ->
      runIO (simpleHTTP $ Request uri GET [] "") >>= \case
        Left err   -> error $ "Error fetching apidoc spec: " ++ show err
        Right resp -> case rspCode resp of
          (2, _, _) -> parse . rspBody $ resp
          _         -> error $ "Error fetching apidoc spec: " ++ show resp

-- TODO: Generate deprecation warnings.
--         requires: https://ghc.haskell.org/trac/ghc/ticket/13551#ticket
-- TODO: Better errors (on name conflicts)
gen :: T.Service -> DecsQ
gen T.Service{..}
  = fmap (concat . concat) . sequence $
      [ forM models $ \m ->
          (++) <$> sequence [ mkData m, mkDataToJSON m, mkDataFromJSON m ] <*> mkDataLens m
      , forM unions $ sequence . flip map [ mkUnion, mkUnionToJSON, mkUnionFromJSON ] . flip ($)
      , forM enums  $ sequence . flip map [ mkEnum , mkEnumToJSON , mkEnumFromJSON  ] . flip ($)
      , forM _serviceResources mkApi
      ]
  where
    models = map model' _serviceModels
    unions = map union' _serviceUnions
    enums  = map enum'  _serviceEnums

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
