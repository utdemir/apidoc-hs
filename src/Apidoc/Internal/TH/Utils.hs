{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Apidoc.Internal.TH.Utils where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
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
--------------------------------------------------------------------------------
import           Apidoc.Internal.TH.Types
--------------------------------------------------------------------------------

mkData :: Data -> Dec
mkData (Data (Nm nm) fs)
  = DataD [] name [] Nothing [RecC name fields] derivings
  where
    name = mkName $ renderType nm
    fields = flip map fs $ \(Nm fnm, ty, tyRequired) ->
      ( mkName $ renderField nm fnm
      , Bang NoSourceUnpackedness NoSourceStrictness
      , let type_ = tyToType ty
        in  if tyRequired
              then type_
              else AppT (ConT ''Maybe) type_
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
      [(ValD (VarP 'parseJSON) (NormalB body) [])]
  where
    body
      = AppE (AppE (VarE 'withObject) (LitE . StringL $ renderType nm)) . LamE [VarP (mkName "obj")] . DoE $
          [ BindS (VarP . mkName $ renderField nm fnm)
                  (InfixE (Just . VarE $ mkName "obj")
                          (VarE $ if req then '(.:)  else '(.:?))
                          (Just . (AppE $ VarE 'T.pack) . LitE . StringL $ fnm))
          | (Nm fnm, _, req) <- fs
          ]
          ++
          [ NoBindS $ AppE (VarE 'return) $ RecConE (mkName $ renderType nm)
              [ (name, VarE name) | (Nm fnm, _, _) <- fs
              , let name = mkName $ renderField nm fnm
              ]
          ]

mkEnumFromJSON :: Enum -> Dec
mkEnumFromJSON (Enum (Nm nm) fs)
  = InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT . mkName $ renderType nm))
      [(ValD (VarP 'parseJSON) (NormalB body) [])]
  where
    body
      = AppE (AppE (VarE 'withText) (LitE . StringL $ renderType nm)) . LamE [VarP $ mkName "val"] $
          AppE (AppE (AppE (VarE 'maybe) (AppE (VarE 'fail) err)) (VarE 'return)) $
            AppE (AppE (VarE 'lookup) (VarE $ mkName "val")) (ListE $ map tup fs)

    tup (Nm x) = TupE [ AppE (VarE 'T.pack) (LitE . StringL $ renderType x)
                      , ConE . mkName $ renderType nm ++ renderType x
                      ]
    err = AppE (AppE
                  (VarE '(++))
                  (AppE (AppE (VarE '(++))
                     (LitE $ StringL "Error parsing "))
                     (AppE (VarE 'T.unpack) (VarE $ mkName "val"))
                  ))
               (LitE . StringL $ ": not a valid " ++ renderType nm)

mkUnionFromJSON :: Union -> Dec
mkUnionFromJSON (Union (Nm nm) ts)
  = InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT . mkName $ renderType nm))
      [(ValD (VarP 'parseJSON) (NormalB body) [])]
  where
    body
      = AppE (AppE (VarE 'withObject) (LitE . StringL $ renderType nm)) . LamE [VarP (mkName "obj")] $
          CaseE (AppE (VarE 'HM.toList) (VarE $ mkName "obj"))
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
                         (Just $ AppE (VarE 'parseJSON) (VarE (mkName "val")))
                     )
                   | (Ty ty) <- ts
                   ]
                )
                []
            , Match WildP (NormalB $ AppE (VarE 'fail) (LitE . StringL $ "invalid union")) []
            ]

--------------------------------------------------------------------------------

mkDataToJSON :: Data -> Dec
mkDataToJSON (Data nm fs) = undefined

mkEnumToJSON :: Enum -> Dec
mkEnumToJSON = undefined

mkUnionToJSON :: Union -> Dec
mkUnionToJSON = undefined

--------------------------------------------------------------------------------

renderType :: String -> String
renderType = pascal

renderField :: String -> String -> String
renderField modelName fieldName
  = "_" ++ camel modelName ++ pascal fieldName

--------------------------------------------------------------------------------

tyToType :: Ty -> Type
tyToType (Ty ty)
  = case parseOnly parser (BS8.pack ty) of
      Left err -> error $ "Erorr parsing type: " ++ ty ++ ". Error: " ++ err
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
    customParser = ConT . mkName . pascal . BS8.unpack <$> A.takeWhile (inClass "a-zA-Z0-9_")

derivings :: Cxt
derivings = map ConT [''Show, ''Eq, ''Generic, ''Typeable]

