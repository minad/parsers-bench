{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Json.PariPariHi
  ( parseJson )
where

import Control.Applicative
import ParsersBench.Json.Common
import Text.PariPari
import qualified Data.HashMap.Strict as H
import qualified Data.Vector         as V
import qualified Data.Scientific     as Sci

parseJson :: ByteString -> Value
parseJson bs =
  case runParser json "" bs of
    Left err -> error (show err)
    Right x -> x

json :: Parser p => p Value
json = space *> (object <|> array)

object :: Parser p => p Value
object = Object . H.fromList <$> (char '{' *> space *> sepBy pair (space *> char ',' *> space) <* space <* char '}')
  where pair = liftA2 (,) (jstring <* space) (char ':' *> space *> value)

array :: Parser p => p Value
array = Array . V.fromList <$> (char '[' *> sepBy value (space *> char ',' *> space) <* space <* char ']')

value :: Parser p => p Value
value = do
  (String <$> jstring)
    <|> object
    <|> array
    <|> (Bool False <$ string "false")
    <|> (Bool True  <$ string "true")
    <|> (Null       <$ string "null")
    <|> (Number     <$> scientific)

jstring :: Parser p => p Text
jstring = char '"' *> asString (skipMany $ satisfy (/= '"')) <* char '"'

space :: Parser p => p ()
space = skipMany (satisfy (\c -> c == ' ' || c == '\n' || c == '\t'))

scientific :: Parser p => p Sci.Scientific
scientific = do
  neg <- option id $ negate <$ char '-'
  (c, _, e) <- fractionDec (pure ())
  pure $ Sci.scientific (neg c) (fromIntegral e)
