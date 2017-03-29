{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Apidoc.Internal.Stage1.Types where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Map as M
import Data.Text
import Text.Show.Pretty
--------------------------------------------------------------------------------

data UnionType
  = UnionType
      { _unionTypeType :: Text
      }
  deriving (Show, Eq, Ord)

instance FromJSON UnionType where
  parseJSON = withObject "UnionType" $ \obj ->
    UnionType <$> obj .: "type"

--------------------------------------------------------------------------------

data Union
  = Union
      { _unionName  :: Text
      , _unionTypes :: [UnionType]
      }
  deriving (Show, Eq, Ord)

instance FromJSON Union where
  parseJSON = withObject "Union" $ \obj ->
    Union <$> obj .: "name" <*> obj .: "types"

--------------------------------------------------------------------------------

data EnumValue
  = EnumValue
      { _enumValueName :: Text
      }
  deriving (Show, Eq, Ord)

instance FromJSON EnumValue where
  parseJSON = withObject "EnumValue" $ \obj ->
    EnumValue <$> obj .: "name"

--------------------------------------------------------------------------------

data Enum
  = Enum
      { _enumName   :: Text
      , _enumValues :: [EnumValue]
      }
  deriving (Show, Eq, Ord)

instance FromJSON Enum where
  parseJSON = withObject "Enum" $ \obj ->
    Enum <$> obj .: "name" <*> obj .: "values"

--------------------------------------------------------------------------------

data Model
  = Model
     { _modelName   :: Text
     , _modelFields :: [ModelField]
     }
  deriving (Show, Eq, Ord)

instance FromJSON Model where
  parseJSON = withObject "Model" $ \obj ->
    Model <$> obj .: "name" <*> obj .: "fields"

--------------------------------------------------------------------------------

data ModelField
  = ModelField
      { _modelFieldName     :: Text
      , _modelFieldType     :: Text
      , _modelFieldRequired :: Bool
      }
  deriving (Show, Eq, Ord)

instance FromJSON ModelField where
  parseJSON = withObject "ModelField" $ \obj ->
    ModelField <$> obj .:  "name"
               <*> obj .:  "type"
               <*> obj .:? "required" .!= False

--------------------------------------------------------------------------------

data Api =
  Api
    { _apiEnums  :: [Enum]
    , _apiUnions :: [Union]
    , _apiModels :: [Model]
    }  
  deriving (Show, Eq, Ord)

instance FromJSON Api where
  parseJSON = withObject "Api" $ \obj ->
    Api <$> obj .:? "enums"  .!= []
        <*> obj .:? "unions" .!= []
        <*> obj .:? "models" .!= []

--------------------------------------------------------------------------------

test :: IO ()
test
  = pPrint =<< eitherDecode @Api <$> BL.readFile "static/apidoc-spec-service.json"



  
