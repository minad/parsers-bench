{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Json.AttoparsecHi
  ( parseJson )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.ByteString (ByteString)
import Data.Text (Text)
import ParsersBench.Json.Common
import Prelude hiding (takeWhile)
import qualified Data.HashMap.Strict as H
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V

parseJson :: ByteString -> Value
parseJson bs =
  case parseOnly json (TE.decodeUtf8 bs) of
    Left err -> error (show err)
    Right x -> x

json :: Parser Value
json = skipSpace *> (object <|> array)

object :: Parser Value
object = Object . H.fromList <$> (char '{' *> skipSpace *> sepBy pair (skipSpace *> char ',' *> skipSpace) <* skipSpace <* char '}')
  where pair = liftA2 (,) (jstring <* skipSpace) (char ':' *> skipSpace *> value)

array :: Parser Value
array = Array . V.fromList <$> (char '[' *> sepBy value (skipSpace *> char ',' *> skipSpace) <* skipSpace <* char ']')

value :: Parser Value
value = do
  (String <$> jstring)
    <|> object
    <|> array
    <|> (Bool False <$ string "false")
    <|> (Bool True  <$ string "true")
    <|> (Null       <$ string "null")
    <|> (Number     <$> scientific)

jstring :: Parser Text
jstring = char '"' *> takeWhile (/= '"') <* char '"'
