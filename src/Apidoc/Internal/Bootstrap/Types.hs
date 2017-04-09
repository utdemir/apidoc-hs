{-# LANGUAGE OverloadedStrings #-}

module Apidoc.Internal.Bootstrap.Types where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text
import           Prelude    hiding (Enum)
--------------------------------------------------------------------------------

data UnionType
  = UnionType
      { unionTypeType :: Text
      }
  deriving (Show, Eq, Ord)

instance FromJSON UnionType where
  parseJSON = withObject "UnionType" $ \obj ->
    UnionType <$> obj .: "type"

--------------------------------------------------------------------------------

data Union
  = Union
      { unionName  :: Text
      , unionTypes :: [UnionType]
      }
  deriving (Show, Eq, Ord)

instance FromJSON Union where
  parseJSON = withObject "Union" $ \obj ->
    Union <$> obj .: "name" <*> obj .: "types"

--------------------------------------------------------------------------------

data EnumValue
  = EnumValue
      { enumValueName :: Text
      }
  deriving (Show, Eq, Ord)

instance FromJSON EnumValue where
  parseJSON = withObject "EnumValue" $ \obj ->
    EnumValue <$> obj .: "name"

--------------------------------------------------------------------------------

data Enum
  = Enum
      { enumName   :: Text
      , enumValues :: [EnumValue]
      }
  deriving (Show, Eq, Ord)

instance FromJSON Enum where
  parseJSON = withObject "Enum" $ \obj ->
    Enum <$> obj .: "name" <*> obj .: "values"

--------------------------------------------------------------------------------

data Model
  = Model
     { modelName   :: Text
     , modelFields :: [ModelField]
     }
  deriving (Show, Eq, Ord)

instance FromJSON Model where
  parseJSON = withObject "Model" $ \obj ->
    Model <$> obj .: "name" <*> obj .: "fields"

--------------------------------------------------------------------------------

data ModelField
  = ModelField
      { modelFieldName     :: Text
      , modelFieldType     :: Text
      , modelFieldRequired :: Bool
      }
  deriving (Show, Eq, Ord)

instance FromJSON ModelField where
  parseJSON = withObject "ModelField" $ \obj ->
    ModelField <$> obj .:  "name"
               <*> obj .:  "type"
               <*> obj .:? "required" .!= False

--------------------------------------------------------------------------------

data Service
  = Service
      { serviceEnums  :: [Enum]
      , serviceUnions :: [Union]
      , serviceModels :: [Model]
      }
  deriving (Show, Eq, Ord)

instance FromJSON Service where
  parseJSON = withObject "Service" $ \obj ->
    Service <$> obj .:? "enums"  .!= []
            <*> obj .:? "unions" .!= []
            <*> obj .:? "models" .!= []
