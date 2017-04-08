{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Apidoc.Internal.TH.Gen where

--------------------------------------------------------------------------------
import Control.Lens.Lens
import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Attoparsec.ByteString as A hiding (match)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import Data.List
import           Data.Functor
import           Data.Int
import           Data.Map                   (Map)
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import           Data.UUID.Types            (UUID)
import           GHC.Generics               (Generic)
import           Language.Haskell.TH
import           Prelude                    hiding (Enum)
import           Text.Casing
import Data.List.Split (splitOn)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
--------------------------------------------------------------------------------
import Apidoc.Internal.Types
import           Apidoc.Internal.TH.Types
--------------------------------------------------------------------------------

mkData :: Data -> DecQ
mkData (Data (Nm nm) fs)
  = dataD (return []) name [] Nothing [recC name fields] derivings
  where
    name = mkName $ renderType nm
    fields = flip map fs $ \(Nm fnm, ty, isReq) -> return
      ( mkName $ renderField nm fnm
      , Bang NoSourceUnpackedness NoSourceStrictness
      , tyToTypeReq ty isReq
      )


mkEnum :: Enum -> DecQ
mkEnum (Enum (Nm nm) fs)
  = dataD (return []) (mkName $ renderType nm) [] Nothing (map mk fs) derivings
  where
    mk (Nm fnm) = recC (mkName $ renderType nm ++ renderType fnm) []

mkUnion :: Union -> DecQ
mkUnion (Union (Nm nm) ts)
  = dataD (return []) (mkName $ renderType nm) [] Nothing (map mk ts) derivings
  where
    mk ty@(Ty fnm) = recC (mkName $ renderType nm ++ renderType fnm) [
      return ( mkName $ renderField nm fnm
             , Bang NoSourceUnpackedness NoSourceStrictness
             , tyToType ty
             )
      ]

--------------------------------------------------------------------------------

mkDataFromJSON :: Data -> DecQ
mkDataFromJSON (Data (Nm nm) fs)
  = instanceD (return []) (appT (conT ''FromJSON) (conT . mkName $ renderType nm))
      [funD 'parseJSON [clause [] (normalB body) []]]
  where
    body
      = appQ 'withObject
         [ litE . StringL $ renderType nm
         , lamE [varP $ mkName "obj"] $
             idiom (conE . mkName $ renderType nm)
               [ (infixE
                    (Just . varE $ mkName "obj")
                    (varE $ if req then '(.:)  else '(.:?))
                    (Just $ appQ 'T.pack [litE . StringL $ fnm]))
               | (Nm fnm, _, req) <- fs
               ]
         ]

mkEnumFromJSON :: Enum -> DecQ
mkEnumFromJSON (Enum (Nm nm) fs)
  = instanceD (return []) (appT (conT ''FromJSON) (conT . mkName $ renderType nm))
      [(valD (varP 'parseJSON) (normalB body) [])]
  where
    body
      = appQ 'withText
         [ litE $ StringL (renderType nm)
         , lamE [varP $ mkName "val"] $
             appQ 'maybe
               [ appQ 'fail [err]
               , varE 'return
               , appQ 'lookup [varE $ mkName "val", listE $ map tup fs]
               ]
         ]

    tup (Nm x) = tupE [ appE (varE 'T.pack) (litE $ StringL x)
                      , conE . mkName $ renderType nm ++ renderType x
                      ]

    err = appQ 'concat
      [ listE
          [ litE $ StringL "Error parsing "
          , appQ 'T.unpack [varE $ mkName "val"]
          , litE . StringL $ ": not a valid " ++ renderType nm
          ]
      ]

mkUnionFromJSON :: Union -> DecQ
mkUnionFromJSON (Union (Nm nm) ts)
  = instanceD (return []) (appT (conT ''FromJSON) (conT . mkName $ renderType nm))
      [funD 'parseJSON [clause [] (normalB body) []]]
  where
    body
      = appQ 'withObject
          [ litE . StringL $ renderType nm
          , lamE [varP (mkName "obj")] $
              caseE (appQ 'HM.toList [varE $ mkName "obj"])
                [ match
                    (listP [tupP [varP (mkName "tag"), varP (mkName "val")]])
                    (guardedB
                       [ return
                           ( NormalG (
                               InfixE
                                 (Just . VarE $ mkName "tag")
                                 (VarE '(==))
                                 (Just . AppE (VarE 'T.pack) . LitE . StringL $ ty))
                           , InfixE
                               (Just . ConE . mkName $ renderType nm ++ renderType ty)
                               (VarE ('(<$>)))
                               (Just $ app 'withObject
                                  [ LitE . StringL $ renderType nm
                                  , LamE [VarP (mkName "val")]
                                         (InfixE
                                           (Just . VarE $ mkName "val")
                                           (VarE '(.:))
                                           (Just $ app 'T.pack [LitE $ StringL "value"]))
                                  , VarE $ mkName "val"
                                  ]
                               )
                           )
                       | (Ty ty) <- ts
                       ]
                    )
                    []
                , match (return WildP) (normalB $ appQ 'fail [litE . StringL $ "invalid union"]) []
                ]
          ]

--------------------------------------------------------------------------------

mkDataToJSON :: Data -> DecQ
mkDataToJSON (Data (Nm nm) fs)
  = instanceD (return []) (appT (conT ''ToJSON) (conT . mkName $ renderType nm))
      [funD 'toJSON [clause [varP $ mkName "val"] (normalB body) []]]
  where
    body
      = appE (varE 'object) $ listE
          [ tupE
              [ appQ 'T.pack [litE $ StringL fnm]
              , appQ 'toJSON [appQ (mkName $ renderField nm fnm) [varE $ mkName "val"]]
              ]
          | (Nm fnm, _, req) <- fs
          , req
          ]

mkEnumToJSON :: Enum -> DecQ
mkEnumToJSON (Enum (Nm nm) ts)
  = instanceD (return []) (appT (conT ''ToJSON) (conT . mkName $ renderType nm))
      [funD 'toJSON [clause [varP $ mkName "val"] (normalB body) []]]
  where
    body
      = caseE (varE $ mkName "val")
          [ match
              (conP (mkName $ renderType nm ++ renderType t) [])
              (normalB $ appE (conE 'String) (appQ 'T.pack [litE $ StringL t]))
              []
          | Nm t <- ts
          ]

mkUnionToJSON :: Union -> DecQ
mkUnionToJSON (Union (Nm nm) fs)
  = instanceD (return []) (appT (conT ''ToJSON) (conT . mkName $ renderType nm))
      [funD 'toJSON [clause [varP $ mkName "val"] (normalB body) []]]
  where
    body
      = caseE (varE $ mkName "val")
          [ match
              (conP (mkName $ renderType nm ++ renderType ty) [varP $ mkName "ty"])
              (normalB $ appE (varE 'object) (listE [
                 tupE
                   [ appQ 'T.pack [litE . StringL $ ty]
                   , appQ 'object
                       [ listE
                         [ tupE [ appE (varE 'T.pack) (litE $ StringL "value")
                                , appE (varE 'toJSON) (varE $ mkName "ty")
                                ]
                         ]
                       ]
                   ]
              ]))
              []
          | Ty ty <- fs
          ]

--------------------------------------------------------------------------------

mkDataLens :: Data -> DecsQ
mkDataLens (Data (Nm nm) fs)
  = sequence . concat $
      [ [ sigD lensName sig
        , valD (varP lensName) (normalB body) []
        ]
      | (Nm fnm, ty, isReq) <- fs
      , let lensName = mkName . drop 1 $ renderField nm fnm
      , let sig = appTypeQ ''Lens' [ conT . mkName $ renderType nm
                                   , return $ tyToTypeReq ty isReq
                                   ]
      , let body = appQ 'lens
              [ varE . mkName $ renderField nm fnm
              , lamE [ varP $ mkName "s", varP $ mkName "a" ] $
                  recUpdE (varE $ mkName "s")
                   [ return (mkName $ renderField nm fnm, VarE $ mkName "a") ]
              ]
      ]

--------------------------------------------------------------------------------

renderType :: String -> String
renderType = pascal . last . splitOn "."

renderField :: String -> String -> String
renderField modelName fieldName
  = "_" ++ camel (renderType modelName) ++ pascal fieldName

--------------------------------------------------------------------------------

tyToTypeReq :: Ty -> Bool -> Type
tyToTypeReq ty req
  = (if req then id else (AppT (ConT ''Maybe))) $ tyToType ty

tyToType :: Ty -> Type
tyToType (Ty ty)
  = case parseOnly (parser <* endOfInput) (BS8.pack ty) of
      Left err -> error $ "Erorr parsing type: " ++ ty ++ ". Error: " ++ err
      Right ty -> ty
  where
    parser :: Parser Type
    parser
        = "boolean"           $> ConT ''Bool
      <|> "date-iso8601"      $> ConT ''Day
      <|> "date-time-iso8601" $> ConT ''UTCTime
      <|> "decimal"           $> ConT ''Rational
      <|> "double"            $> ConT ''Double
      <|> "integer"           $> ConT ''Int32
      <|> "long"              $> ConT ''Int64
      <|> "object"            $> ConT ''Object
      <|> "string"            $> ConT ''T.Text
      <|> "unit"              $> ConT ''()
      <|> "uuid"              $> ConT ''T.Text
      <|> listParser
      <|> mapParser
      <|> customParser

    listParser :: Parser Type
    listParser = AppT (ConT ''[]) <$> ("[" *> parser <* "]")

    mapParser :: Parser Type
    mapParser = AppT (AppT (ConT ''Map) (ConT ''String)) <$> ("map[" *> parser <* "]")

    customParser :: Parser Type
    customParser = ConT . mkName . renderType . BS8.unpack <$> A.takeWhile (inClass "a-zA-Z0-9_.")

derivings :: CxtQ
derivings
  = let defaultExts = map ConT [''Show, ''Eq, ''Typeable]
    in  isExtEnabled DeriveGeneric >>= return . \case
          True  -> ConT ''Generic : defaultExts
          False -> defaultExts

--------------------------------------------------------------------------------

-- | app f [p1, p2, p3] == [| f p1 p2 p3 |]
app :: Name -> [Exp] -> Exp
app = foldl' AppE . VarE

appQ :: Name -> [ExpQ] -> ExpQ
appQ = foldl' appE . return . VarE

appType :: Name -> [Type] -> Type
appType = foldl' AppT . ConT

appTypeQ :: Name -> [TypeQ] -> TypeQ
appTypeQ = foldl' appT . return . ConT


-- | idiom f [p1, p2, p3] == [| f <$> p1 <*> p2 <*> p3 |]
idiom :: Q Exp -> [Q Exp] -> Q Exp
idiom con []     = con
idiom con (p:ps) = go (infixE (Just con) (varE '(<$>)) (Just p)) ps
  where
    go p []     = p
    go p (x:xs) = go (infixE (Just p) (varE '(<*>)) (Just x)) xs
