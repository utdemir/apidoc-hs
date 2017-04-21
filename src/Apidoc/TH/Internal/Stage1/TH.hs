{-# LANGUAGE RecordWildCards   #-}

module Apidoc.TH.Internal.Stage1.TH
  ( stage1
  ) where

--------------------------------------------------------------------------------
import           Data.Aeson                      (eitherDecode)
import qualified Data.ByteString.Lazy            as BL
import qualified Data.Text                       as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Prelude                         hiding (Enum)
--------------------------------------------------------------------------------
import qualified Apidoc.TH.Internal.Stage1.Types as T
import           Apidoc.TH.Internal.Gen.Simple
--------------------------------------------------------------------------------

stage1 :: FilePath -> DecsQ
stage1 serviceSpec = do
  addDependentFile serviceSpec
  json <- runIO (BL.readFile serviceSpec)
  case eitherDecode json of
    Left err  -> error $ "Parser error on " ++ serviceSpec ++ ": " ++ err
    Right api -> stage1' api

stage1' :: T.Service -> DecsQ
stage1' service
  = fmap (concat . concat) . sequence $
      [ mapM renderModel (T.serviceModels service)
      , mapM renderUnion (T.serviceUnions service)
      , mapM renderEnum  (T.serviceEnums service)
      ]

renderModel :: T.Model -> DecsQ
renderModel T.Model{..}
  = sequence [ mkData d, mkDataFromJSON d ]
  where
    d = Data (Nm $ T.unpack modelName) . flip map modelFields
          $ \T.ModelField{..} -> ( Nm $ T.unpack modelFieldName
                                 , Ty $ T.unpack modelFieldType
                                 , modelFieldRequired
                                 )

renderUnion :: T.Union -> DecsQ
renderUnion T.Union{..}
  = sequence [ mkUnion u, mkUnionFromJSON u ]
  where
    u = Union (Nm $ T.unpack unionName)
          $ map (Ty . T.unpack . T.unionTypeType) unionTypes

renderEnum :: T.Enum -> DecsQ
renderEnum T.Enum{..}
  = sequence [ mkEnum e, mkEnumFromJSON e ]
  where
    e = Enum (Nm $ T.unpack enumName)
          $ map (Nm . T.unpack . T.enumValueName) enumValues
