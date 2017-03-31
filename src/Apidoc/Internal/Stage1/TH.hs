{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Apidoc.Internal.Stage1.TH where

--------------------------------------------------------------------------------
import Prelude hiding (Enum)
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
import Control.Lens
import GHC.Generics (Generic)
import Data.Typeable
--------------------------------------------------------------------------------
import Apidoc.Internal.Stage1.Types
--------------------------------------------------------------------------------

stage1 :: FilePath -> DecsQ
stage1 serviceSpec = do
  json <- runIO (BL.readFile serviceSpec)
  case eitherDecode json of
    Left err  -> error $ "Parser error on " ++ serviceSpec ++ ": " ++ err
    Right api -> stage1' api

stage1' :: Api -> DecsQ
stage1' api
  = fmap (concat . concat) . sequence
      $ [ mapM renderModel (_apiModels api)
        , mapM renderUnion (_apiUnions api)
        , mapM renderEnum  (_apiEnums api)
        ]

derivings :: Cxt
derivings = map ConT [''Show, ''Eq, ''Generic, ''Typeable]

renderModel :: Model -> DecsQ
renderModel model
  = (:) <$> return (DataD [] name [] Nothing [RecC name fields] derivings)
        <*> return [] -- makeLenses name
  where
    name = mkName . renderModelName $ _modelName model
    
    fields :: [VarBangType]
    fields = map mkField (_modelFields model)

    mkField :: ModelField -> VarBangType
    mkField field =
      ( mkName $ renderFieldName (_modelName model) (_modelFieldName field)
      , Bang NoSourceUnpackedness NoSourceStrictness
      , let ty = typeNameToType $ _modelFieldType field 
        in  if _modelFieldRequired field
              then ty
              else AppT (ConT ''Maybe) ty
      )

renderFieldName :: T.Text -> T.Text -> String
renderFieldName modelName fieldName
  = "_" ++ camel (T.unpack modelName) ++ pascal (T.unpack fieldName)

renderModelName :: T.Text -> String
renderModelName = pascal . T.unpack

typeNameToType :: T.Text -> Type
typeNameToType typeName
  = case parseOnly parser typeName of
      Left err -> error $ "Erorr parsing type: " ++ T.unpack typeName ++ ". Error: " ++ err
      Right ty -> ty
  where
    parser :: Parser Type
    parser
        = "boolean"           $> ConT ''Bool
      <|> "date-iso8601"      $> ConT ''()
      <|> "date-time-iso8601" $> ConT ''()
      <|> "decimal"           $> ConT ''Rational
      <|> "double"            $> ConT ''Double
      <|> "integer"           $> ConT ''Int32
      <|> "long"              $> ConT ''Int64
      <|> "object"            $> ConT ''Object
      <|> "string"            $> ConT ''T.Text
      <|> "unit"              $> ConT ''()
      <|> "uuid"              $> ConT ''UUID
      <|> listParser
      <|> mapParser
      <|> customParser

    listParser :: Parser Type
    listParser = AppT (ConT ''[]) <$> ("[" *> parser <* "]")
    
    mapParser :: Parser Type
    mapParser = AppT (AppT (ConT ''Map) (ConT ''String)) <$> ("map[" *> parser <* "]")

    customParser :: Parser Type
    customParser = ConT . mkName . pascal . T.unpack <$> A.takeWhile (inClass "a-zA-Z0-9_")

renderUnion :: Union -> DecsQ
renderUnion union
  = return [DataD [] (mkName name) [] Nothing (map mkValue $ _unionTypes union) derivings]
  where
    name = renderModelName $ _unionName union
    
    mkValue val = RecC (mkName $ name ++ (pascal . T.unpack) (_unionTypeType val)) [
      ( mkName $ renderFieldName (_unionName union) (_unionTypeType val)
      , Bang NoSourceUnpackedness NoSourceStrictness
      , typeNameToType $ _unionTypeType val
      )]
      
renderEnum :: Enum -> DecsQ
renderEnum enum
  = return [DataD [] (mkName name) [] Nothing (map mkValue $ _enumValues enum)  derivings]
  where
    name = renderModelName $ _enumName enum
    mkValue val = RecC (mkName $ name ++ (pascal . T.unpack) (_enumValueName val)) []
  
--------------------------------------------------------------------------------

test :: IO ()
test = do
  decs <- runQ $ stage1 "static/apidoc-spec-service.json"
  print $ ppr decs

