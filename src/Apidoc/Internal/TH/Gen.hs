{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Apidoc.Internal.TH.Gen where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens.Lens
import           Data.Aeson
import           Data.Attoparsec.Text     as A hiding (match)
import           Data.Functor
import qualified Data.HashMap.Strict      as HM
import           Data.Int
import           Data.List
import           Data.List.Split          (splitOn)
import           Data.Map                 (Map)
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Time.Calendar       (Day)
import           Data.Time.Clock          (UTCTime)
import           Data.Typeable            (Typeable)
import           GHC.Generics             (Generic)
import           Language.Haskell.TH
import           Prelude                  hiding (Enum)
import           Text.Casing
--------------------------------------------------------------------------------
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
      = app 'withObject
         [ litE . StringL $ renderType nm
         , lamE [varP $ mkName "obj"] $
             idiom (conE . mkName $ renderType nm)
               [ (infixE
                    (Just . varE $ mkName "obj")
                    (varE $ if req then '(.:)  else '(.:?))
                    (Just $ app 'T.pack [litE . StringL $ fnm]))
               | (Nm fnm, _, req) <- fs
               ]
         ]

mkEnumFromJSON :: Enum -> DecQ
mkEnumFromJSON (Enum (Nm nm) fs)
  = instanceD (return []) (appT (conT ''FromJSON) (conT . mkName $ renderType nm))
      [(valD (varP 'parseJSON) (normalB body) [])]
  where
    body
      = app 'withText
         [ litE $ StringL (renderType nm)
         , lamE [varP $ mkName "val"] $
             app 'maybe
               [ app 'fail [err]
               , varE 'return
               , app 'lookup [varE $ mkName "val", listE $ map tup fs]
               ]
         ]

    tup (Nm x) = tupE [ appE (varE 'T.pack) (litE $ StringL x)
                      , conE . mkName $ renderType nm ++ renderType x
                      ]

    err = app 'concat
      [ listE
          [ litE $ StringL "Error parsing "
          , app 'T.unpack [varE $ mkName "val"]
          , litE . StringL $ ": not a valid " ++ renderType nm
          ]
      ]

mkUnionFromJSON :: Union -> DecQ
mkUnionFromJSON (Union (Nm nm) ts)
  = instanceD (return []) (appT (conT ''FromJSON) (conT . mkName $ renderType nm))
      [funD 'parseJSON [clause [] (normalB body) []]]
  where
    body
      = app 'withObject
          [ litE . StringL $ renderType nm
          , lamE [varP (mkName "obj")] $
              caseE (app 'HM.toList [varE $ mkName "obj"])
                [ match
                    (listP [tupP [varP (mkName "tag"), varP (mkName "val")]])
                    (guardedB
                       [ (,) <$> normalG (
                                   infixE
                                     (Just . varE $ mkName "tag")
                                     (varE '(==))
                                     (Just . appE (varE 'T.pack) . litE . StringL $ ty))
                             <*> infixE
                                   (Just . conE . mkName $ renderType nm ++ renderType ty)
                                   (varE ('(<$>)))
                                   (Just $ app 'withObject
                                      [ litE . StringL $ renderType nm
                                      , lamE [varP (mkName "obj'")]
                                             (infixE
                                               (Just . varE $ mkName "obj'")
                                               (varE '(.:))
                                               (Just $ app 'T.pack [litE $ StringL "value"]))
                                      , varE $ mkName "val"
                                      ]
                                   )
                       | (Ty ty) <- ts
                       ]
                    )
                    []
                , match (return WildP) (normalB $ app 'fail [litE . StringL $ "invalid union"]) []
                ]
          ]

--------------------------------------------------------------------------------

mkDataToJSON :: Data -> DecQ
mkDataToJSON (Data (Nm nm) fs)
  = instanceD (return []) (appT (conT ''ToJSON) (conT . mkName $ renderType nm))
      [funD 'toJSON [clause [varP $ mkName "val"] (normalB body) []]]
  where
    body
      = appE (varE 'object) . appE (varE 'catMaybes) $ listE
          [ if req
              then appE (conE 'Just) $ tup "val"
              else app 'fmap
                     [ lamE [varP $ mkName "inner"] $ tup "inner"
                     , app (mkName $ renderField nm fnm) [varE $ mkName "val"]
                     ]
          | (Nm fnm, _, req) <- fs
          , let tup val = tupE [ app 'T.pack [litE $ StringL fnm]
                               , app 'toJSON [varE $ mkName val]
                               ]
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
              (normalB $ appE (conE 'String) (app 'T.pack [litE $ StringL t]))
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
                   [ app 'T.pack [litE . StringL $ ty]
                   , app 'object
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
            sig = appTy ''Lens' [ conT . mkName $ renderType nm
                                , return $ tyToTypeReq ty isReq
                                ]
            body = app 'lens
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
  = case parseOnly (parser <* endOfInput) (T.pack ty) of
      Left err    -> error $ "Erorr parsing type: " ++ ty ++ ". Error: " ++ err
      Right type_ -> type_
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
    customParser = ConT . mkName . renderType . T.unpack <$> A.takeWhile (inClass "a-zA-Z0-9_.")

derivings :: CxtQ
derivings
  = let defaultExts = map ConT [''Show, ''Eq, ''Typeable]
    in  isExtEnabled DeriveGeneric >>= return . \case
          True  -> ConT ''Generic : defaultExts
          False -> defaultExts

--------------------------------------------------------------------------------

app :: Name -> [ExpQ] -> ExpQ
app = foldl' appE . return . VarE

appTy :: Name -> [TypeQ] -> TypeQ
appTy = foldl' appT . return . ConT

-- | idiom f [p1, p2, p3] == [| f <$> p1 <*> p2 <*> p3 |]
idiom :: Q Exp -> [Q Exp] -> Q Exp
idiom con []     = con
idiom con (p:ps) = go (infixE (Just con) (varE '(<$>)) (Just p)) ps
  where
    go a []     = a
    go a (x:xs) = go (infixE (Just a) (varE '(<*>)) (Just x)) xs
