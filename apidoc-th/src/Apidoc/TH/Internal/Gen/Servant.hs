{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Apidoc.TH.Internal.Gen.Servant
  ( mkApi
  ) where

import qualified Data.Text as T
import Language.Haskell.TH
import Servant.API
import Data.List.Split
import Text.Casing
import Data.Proxy

import Apidoc.TH.Internal.Gen.Simple.Types
import Apidoc.TH.Internal.Gen.Utils
import Apidoc.TH.Internal.Stage1

-- TODO: Remove -XDataKinds and -XTypeOperators dependency
mkApi :: Resource -> DecsQ
mkApi res@Resource {..} =
  sequence
    [ tySynD tName [] (opsToType res)
    , sigD fName (appT (conT ''Proxy) (conT $ mkName name))
    , funD fName [clause [] (normalB $ conE 'Proxy) []]
    ]
  where
    name = renderType (T.unpack _resourceType) ++ "Api"
    tName = mkName name
    fName = mkName $ camel name

opsToType :: Resource -> Q Type
opsToType res@Resource {..} =
  case map (parensT . opToType res) _resourceOperations of
    [] -> conT ''()
    (x:xs) -> foldl (flip uInfixT orName) x xs

opToType :: Resource -> Operation -> Q Type
opToType _ op =
  case concatMap
         ($ op)
         [path, parameters, maybe [] pure . body, pure . response] of
    [] -> error "invariant violation"
    x:xs -> foldl (flip uInfixT sepName) x xs

-- TODO: Path parameters
path :: Operation -> [Q Type]
path = map (litT . strTyLit) . splitPath . T.unpack . _operationPath

parameters :: Operation -> [Q Type]
parameters Operation {..} =
  flip map _operationParameters $ \Parameter {..} ->
    appTy
      ''QueryParam
      [ litT . strTyLit $ T.unpack _parameterName
      , return . tyToType . Ty $ T.unpack _parameterType
      ]

body :: Operation -> Maybe (Q Type)
body = const Nothing

response :: Operation -> Q Type
response Operation {..} =
  case filter
         (\Response {..} ->
            case _responseCode of
              ResponseCodeInteger i -> i >= 200 && i < 300
              ResponseCodeResponseCodeOption _ -> False) $
       _operationResponses of
    Response {..}:_ ->
      appTy
        (methodToTy _operationMethod)
        [cts, return . tyToType . Ty $ T.unpack _responseType]
    [] ->
      error $
      "Currently Servant API generation requires at least one 2xx response. "
  where
    cts = promotedConsT `appT` conT ''JSON `appT` promotedNilT

methodToTy :: Method -> Name
methodToTy MethodGet = ''Get
methodToTy MethodPost = ''Post
methodToTy MethodPut = ''Put
methodToTy MethodDelete = ''Delete
methodToTy t = error $ "Unsupported method: " ++ show t

splitPath :: String -> [String]
splitPath = filter (not . null) . splitOn "/"

-- FIXME: Inline these, waiting for
-- https://github.com/commercialhaskell/hindent/issues/412
sepName, orName :: Name
sepName = ''(:>)
orName = ''(:<|>)
