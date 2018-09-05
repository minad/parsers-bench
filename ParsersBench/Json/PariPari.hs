{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module ParsersBench.Json.PariPari
  ( parseJson )
where

import Control.Applicative
import Data.Vector (Vector)
import ParsersBench.Json.Common
import Text.PariPari
import qualified Data.HashMap.Strict as H
import qualified Data.Scientific     as Sci
import qualified Data.Vector         as V

parseJson :: ByteString -> Value
parseJson bs =
  case runParser json "" bs of
    Left err -> error (show err)
    Right x -> x

json :: Parser p => p Value
json = json_ object_ array_

json_ :: Parser p => p Value -> p Value -> p Value
json_ obj ary = do
  w <- space *> (char '{' <|> char '[')
  if w == '{'
    then obj
    else ary
{-# INLINE json_ #-}

object_ :: Parser p => p Value
object_ = Object <$> objectValues jstring value

objectValues :: Parser p => p Text -> p Value -> p (H.HashMap Text Value)
objectValues str val = do
  space
  let pair = liftA2 (,) (str <* space) (char ':' *> space *> val)
  H.fromList <$> commaSeparated pair '}'
{-# INLINE objectValues #-}

array_ :: Parser p => p Value
array_ = Array <$> arrayValues value

arrayValues :: Parser p => p Value -> p (Vector Value)
arrayValues val = do
  space
  V.fromList <$> commaSeparated val ']'
{-# INLINE arrayValues #-}

commaSeparated :: Parser p => p a -> Char -> p [a]
commaSeparated item endByte = do
  w <- lookAhead anyChar
  if w == endByte
    then [] <$ anyChar
    else loop
  where
    loop = do
      v <- item <* space
      ch <- char ',' <|> char endByte
      if ch == ','
        then space >> (v:) <$> loop
        else return [v]
{-# INLINE commaSeparated #-}

value :: Parser p => p Value
value = do
  w <- lookAhead anyChar
  case w of
    '"' -> anyChar *> (String <$> jstring_)
    '{' -> anyChar *> object_
    '[' -> anyChar *> array_
    'f' -> Bool False <$ string "false"
    't' -> Bool True  <$ string "true"
    'n' -> string "null" *> pure Null
    _
      | w >= '0' && w <= '9' || w == '-' -> Number <$> scientific
      | otherwise -> fail "not a valid json value"

jstring :: Parser p => p Text
jstring = char '"' *> jstring_

jstring_ :: Parser p => p Text
jstring_ =
  asString (skipMany $ satisfy (/= '"')) <* char '"'
{-# INLINE jstring_ #-}

space :: Parser p => p ()
space = skipMany (satisfy (\c -> c == ' ' || c == '\n' || c == '\t'))

scientific :: Parser p => p Sci.Scientific
scientific = do
  neg <- option id $ negate <$ char '-'
  (c, _, e) <- fractionDec (pure ())
  pure $ Sci.scientific (neg c) (fromIntegral e)
