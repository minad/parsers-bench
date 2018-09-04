{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Json.MegaparsecHi
  ( parseJson )
where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Void
import ParsersBench.Json.Common
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.HashMap.Strict        as H
import qualified Data.Text.Encoding         as TE
import qualified Data.Vector                as V
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseJson :: ByteString -> Value
parseJson bs =
  case parse json "" (TE.decodeUtf8 bs) of
    Left err -> error (show err)
    Right x -> x

json :: Parser Value
json = space *> (object <|> array)

object :: Parser Value
object = Object . H.fromList <$> (char '{' *> space *> sepBy pair (try (space *> char ',') *> space) <* space <* char '}')
  where pair = liftA2 (,) (jstring <* space) (char ':' *> space *> value)

array :: Parser Value
array = Array . V.fromList <$> (char '[' *> sepBy value (try (space *> char ',') *> space) <* space <* char ']')

value :: Parser Value
value = do
  (String <$> jstring)
    <|> object
    <|> array
    <|> (Bool False <$ string "false")
    <|> (Bool True  <$ string "true")
    <|> (Null       <$ string "null")
    <|> (Number     <$> L.scientific)

jstring :: Parser Text
jstring = char '"' *> takeWhileP (Just "string char") (/= '"') <* char '"'
