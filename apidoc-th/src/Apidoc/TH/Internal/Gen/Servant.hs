{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Apidoc.TH.Internal.Gen.Servant
  ( mkApi
  ) where

import qualified Data.Text as T
import Language.Haskell.TH
import Servant.API
import Data.List.Split

import Apidoc.TH.Internal.Gen.Utils
import Apidoc.TH.Internal.Stage1

-- TODO: Remove -XDataKinds and -XTypeOperators dependency
mkApi :: Resource -> DecsQ
mkApi Resource {..} =
  sequence
    [ tySynD
        (mkName . ("Api" ++) . renderType . T.unpack $ _resourceType)
        []
        (case map (parensT . opToType) _resourceOperations of
           [] -> conT ''()
           (x:xs) -> foldl (flip uInfixT orName) x xs)
    ]
  where
    basePath = maybe [] (splitPath . T.unpack) _resourcePath
    -- TODO: Path parameters
    paths =
      map (litT . strTyLit) .
      (basePath ++) . splitPath . T.unpack . _operationPath
    parameters op = [conT ''()]
    body op = Just $ conT ''()
    response op = conT ''()
    opToType op =
      case concatMap
             ($ op)
             [paths, parameters, maybe [] pure . body, pure . response] of
        [] -> error "invariant violation"
        x:[] -> x
        x:xs -> foldl (flip uInfixT sepName) x xs


splitPath :: String -> [String]
splitPath s =
  case splitOn "/" s of
    [] -> []
    "":[] -> []
    xs -> xs

-- FIXME: Inline these, waiting for
-- https://github.com/commercialhaskell/hindent/issues/412
sepName, orName :: Name
sepName = ''(:>)
orName = ''(:<|>)
