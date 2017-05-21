{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Apidoc.TH.Internal.Gen.Utils where

--------------------------------------------------------------------------------
import Language.Haskell.TH
import Data.Aeson
import Data.List
import Data.Functor
import Control.Applicative
import Text.Casing
import Data.List.Split (splitOn)
import qualified Data.Text as T
import Data.Attoparsec.Text as A hiding (match)
import Data.Map (Map)
import Data.Int
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
--------------------------------------------------------------------------------
import Apidoc.TH.Internal.Gen.Simple.Types
--------------------------------------------------------------------------------

renderType :: String -> String
renderType = pascal . last . splitOn "."

renderField :: String -> String -> String
renderField modelName fieldName
  = "_" ++ camel (renderType modelName) ++ pascal fieldName

renderEnumValue :: String -> String -> String
renderEnumValue name field
  = renderType name ++ pascal (replace '.' '_' field)
  where
    replace :: Eq a => a -> a -> [a] -> [a]
    replace _ _ [] = []
    replace src target (x:xs) =  (if x == src then target else x) : replace src target xs

tyToTypeReq :: Ty -> Bool -> Type
tyToTypeReq ty req
  = (if req then id else AppT (ConT ''Maybe)) $ tyToType ty

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
    go = foldl $ \ a x -> infixE (Just a) (varE '(<*>)) (Just x)
