{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Apidoc.Internal.TH.Utils where

--------------------------------------------------------------------------------
import Control.Lens.Lens
import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Attoparsec.ByteString as A
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

mkData :: Data -> Dec
mkData (Data (Nm nm) fs)
  = DataD [] name [] Nothing [RecC name fields] derivings
  where
    name = mkName $ renderType nm
    fields = flip map fs $ \(Nm fnm, ty, isReq) ->
      ( mkName $ renderField nm fnm
      , Bang NoSourceUnpackedness NoSourceStrictness
      , tyToTypeReq ty isReq
      )


mkEnum :: Enum -> Dec
mkEnum (Enum (Nm nm) fs)
  = DataD [] (mkName $ renderType nm) [] Nothing (map mk fs) derivings
  where
    mk (Nm fnm) = RecC (mkName $ renderType nm ++ renderType fnm) []

mkUnion :: Union -> Dec
mkUnion (Union (Nm nm) ts)
  = DataD [] (mkName $ renderType nm) [] Nothing (map mk ts) derivings
  where
    mk ty@(Ty fnm) = RecC (mkName $ renderType nm ++ renderType fnm) [
      ( mkName $ renderField nm fnm
      , Bang NoSourceUnpackedness NoSourceStrictness
      , tyToType ty
      )]

--------------------------------------------------------------------------------

mkDataFromJSON :: Data -> Dec
mkDataFromJSON (Data (Nm nm) fs)
  = InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT . mkName $ renderType nm))
      [FunD 'parseJSON [Clause [] (NormalB body) []]]
  where
    body
      = app 'withObject
         [ LitE . StringL $ renderType nm
         , LamE [VarP (mkName "obj")] $
             idiom (ConE . mkName $ renderType nm)
               [ (InfixE
                    (Just . VarE $ mkName "obj")
                    (VarE $ if req then '(.:)  else '(.:?))
                    (Just $ app 'T.pack [LitE . StringL $ fnm]))
               | (Nm fnm, _, req) <- fs
               ]
         ]

mkEnumFromJSON :: Enum -> Dec
mkEnumFromJSON (Enum (Nm nm) fs)
  = InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT . mkName $ renderType nm))
      [(ValD (VarP 'parseJSON) (NormalB body) [])]
  where
    body
      = app 'withText
         [ LitE $ StringL (renderType nm)
         , LamE [VarP $ mkName "val"] $
             app 'maybe
               [ app 'fail [err]
               , VarE 'return
               , app 'lookup [VarE $ mkName "val", ListE $ map tup fs]
               ]
         ]

    tup (Nm x) = TupE [ AppE (VarE 'T.pack) (LitE $ StringL x)
                      , ConE . mkName $ renderType nm ++ renderType x
                      ]

    err = app 'concat
      [ ListE
          [ LitE $ StringL "Error parsing "
          , app 'T.unpack [VarE $ mkName "val"]
          , LitE . StringL $ ": not a valid " ++ renderType nm
          ]
      ]

mkUnionFromJSON :: Union -> Dec
mkUnionFromJSON (Union (Nm nm) ts)
  = InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT . mkName $ renderType nm))
      [FunD 'parseJSON [Clause [] (NormalB body) []]]
  where
    body
      = app 'withObject
          [ LitE . StringL $ renderType nm
          , LamE [VarP (mkName "obj")] $
              CaseE (app 'HM.toList [VarE $ mkName "obj"])
                [ Match
                    (ListP [TupP [VarP (mkName "tag"), VarP (mkName "val")]])
                    (GuardedB
                       [ ( NormalG (
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
                , Match WildP (NormalB $ app 'fail [LitE . StringL $ "invalid union"]) []
                ]
          ]

--------------------------------------------------------------------------------

mkDataToJSON :: Data -> Dec
mkDataToJSON (Data (Nm nm) fs)
  = InstanceD Nothing [] (AppT (ConT ''ToJSON) (ConT . mkName $ renderType nm))
      [FunD 'toJSON [Clause [VarP $ mkName "val"] (NormalB body) []]]
  where
    body
      = AppE (VarE 'object) $ ListE
          [ TupE
              [ app 'T.pack [LitE $ StringL fnm]
              , app 'toJSON [app (mkName $ renderField nm fnm) [VarE $ mkName "val"]]
              ]
          | (Nm fnm, _, req) <- fs
          , req
          ]

mkEnumToJSON :: Enum -> Dec
mkEnumToJSON (Enum (Nm nm) ts)
  = InstanceD Nothing [] (AppT (ConT ''ToJSON) (ConT . mkName $ renderType nm))
      [FunD 'toJSON [Clause [VarP $ mkName "val"] (NormalB body) []]]
  where
    body
      = CaseE (VarE $ mkName "val")
          [ Match
              (ConP (mkName $ renderType nm ++ renderType t) [])
              (NormalB $ AppE (ConE 'String) (app 'T.pack [LitE $ StringL t]))
              []
          | Nm t <- ts
          ]

mkUnionToJSON :: Union -> Dec
mkUnionToJSON (Union (Nm nm) fs)
  = InstanceD Nothing [] (AppT (ConT ''ToJSON) (ConT . mkName $ renderType nm))
      [FunD 'toJSON [Clause [VarP $ mkName "val"] (NormalB body) []]]
  where
    body
      = CaseE (VarE $ mkName "val")
          [ Match
              (ConP (mkName $ renderType nm ++ renderType ty) [VarP $ mkName "ty"])
              (NormalB $ AppE (VarE 'object) (ListE [
                 TupE
                   [ app 'T.pack [LitE . StringL $ ty]
                   , app 'object
                       [ ListE
                         [ TupE [ AppE (VarE 'T.pack) (LitE $ StringL "value")
                                , AppE (VarE 'toJSON) (VarE $ mkName "ty")
                                ]
                         ]
                       ]
                   ]
              ]))
              []
          | Ty ty <- fs
          ]

--------------------------------------------------------------------------------

mkDataLens :: Data -> [Dec]
mkDataLens (Data (Nm nm) fs)
  = concat
    $ [ [ SigD lensName sig
        , ValD (VarP lensName) (NormalB body) []
        ]
      | (Nm fnm, ty, isReq) <- fs
      , let lensName = mkName . drop 1 $ renderField nm fnm
      , let sig = appType ''Lens' [ ConT . mkName $ renderType nm
                                  , tyToTypeReq ty isReq
                                  ]
      , let body = app 'lens
              [ VarE . mkName $ renderField nm fnm
              , LamE [ VarP $ mkName "s", VarP $ mkName "a" ] $
                  RecUpdE (VarE $ mkName "s")
                   [ (mkName $ renderField nm fnm, VarE $ mkName "a") ]
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

-- FIXME: Add `Generic` only when `DeriveGeneric` is enabled
derivings :: Cxt
derivings = map ConT [''Show, ''Eq, ''Generic, ''Typeable]

--------------------------------------------------------------------------------

-- | app f [p1, p2, p3] == [| f p1 p2 p3 |]
app :: Name -> [Exp] -> Exp
app = foldl' AppE . VarE

appType :: Name -> [Type] -> Type
appType = foldl' AppT . ConT


-- | idiom f [p1, p2, p3] == [| f <$> p1 <*> p2 <*> p3 |]
idiom :: Exp -> [Exp] -> Exp
idiom con []     = con
idiom con (p:ps) = go (InfixE (Just con) (VarE '(<$>)) (Just p)) ps
  where
    go p []     = p
    go p (x:xs) = go (InfixE (Just p) (VarE '(<*>)) (Just x)) xs
