{-# LANGUAGE TypeApplications #-}

module Apidoc.Internal.Stage1.TH where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import Language.Haskell.TH
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
  = DataD [] (mkName . renderModelName $ _modelName model) [] Nothing [] []


renderModelName :: T.Text -> String
renderModelName = T.unpack . T.filter (/= '_') . T.toTitle
