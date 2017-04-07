{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Apidoc.Internal.Bootstrap.TH where

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
import GHC.Generics (Generic)
import Data.Typeable
--------------------------------------------------------------------------------
import qualified Apidoc.Internal.Bootstrap.Types as T
import Apidoc.Internal.TH.Utils
import Apidoc.Internal.TH.Types
--------------------------------------------------------------------------------

bootstrap :: FilePath -> DecsQ
bootstrap serviceSpec = do
  json <- runIO (BL.readFile serviceSpec)
  case eitherDecode json of
    Left err  -> error $ "Parser error on " ++ serviceSpec ++ ": " ++ err
    Right api -> bootstrap' api

bootstrap' :: T.Service -> DecsQ
bootstrap' service
  = return . concat . concat $
      [ map renderModel (T.serviceModels service)
      , map renderUnion (T.serviceUnions service)
      , map renderEnum  (T.serviceEnums service)
      ]

renderModel :: T.Model -> [Dec]
renderModel T.Model{..}
  = [ mkData d, mkDataFromJSON d ]
  where
    d = Data (Nm $ T.unpack modelName) . flip map modelFields
          $ \T.ModelField{..} -> ( Nm $ T.unpack modelFieldName
                                 , Ty $ T.unpack modelFieldType
                                 , modelFieldRequired
                                 )

renderUnion :: T.Union -> [Dec]
renderUnion T.Union{..}
  = [ mkUnion u, mkUnionFromJSON u ]
  where
    u = Union (Nm $ T.unpack unionName) 
          $ map (Ty . T.unpack . T.unionTypeType) unionTypes

renderEnum :: T.Enum -> [Dec]
renderEnum T.Enum{..}
  = [ mkEnum e, mkEnumFromJSON e ]
  where
    e = Enum (Nm $ T.unpack enumName) 
          $ map (Nm . T.unpack . T.enumValueName) enumValues

-- test :: IO ()
-- test = do
--   decs <- runQ $ bootstrap "static/apidoc-spec-service.json"
--   print $ print decs
--