{-# LANGUAGE TypeApplications #-}

module Apidoc.Internal.Stage1.TH where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Aeson
import qualified Data.Text as T
--------------------------------------------------------------------------------
import Apidoc.Internal.Stage1.Types
--------------------------------------------------------------------------------

modelsFor :: FilePath -> DecsQ
modelsFor serviceSpec = do
  json <- runIO $ BL.readFile serviceSpec
  let api = case eitherDecode @Api json of
        Left err -> error $ "Error reading " ++ serviceSpec ++ ": " ++ err
        Right xs -> xs
        
  return $ map renderModel (_apiModels api) 

renderModel :: Model -> Dec
renderModel model
  = DataD [] name [] Nothing [RecC name fields] []
  where
    name = mkName . renderModelName $ _modelName model
    
    fields :: [VarBangType]
    fields = map mkField (_modelFields model)

    mkField :: ModelField -> VarBangType
    mkField field =
      ( mkName . T.unpack $ _modelFieldName field
      , Bang NoSourceUnpackedness NoSourceStrictness
      , ConT . mkName . T.unpack $ _modelFieldType field
      )


renderModelName :: T.Text -> String
renderModelName = T.unpack . T.filter (/= '_') . T.toTitle

--------------------------------------------------------------------------------

test :: IO ()
test = do
  decs <- runQ $ modelsFor "static/apidoc-spec-service.json"
  print $ ppr decs

