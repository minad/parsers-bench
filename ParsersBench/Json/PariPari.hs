{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module ParsersBench.Json.PariPari
  ( parseJson )
where

import Control.Applicative
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import ParsersBench.Json.Common
import Text.PariPari
import qualified Data.HashMap.Strict as H
import qualified Data.Scientific     as Sci
import qualified Data.Vector         as V
import qualified Data.Text.Encoding  as T

type StringType = ByteString
type Parser a = (forall p. CharParser StringType p => p a)

parseJson :: ByteString -> Value
parseJson bs =
  case runCharParser json "" bs of
    Left err -> error (show err)
    Right x -> x

json :: Parser Value
json = json_ object_ array_
{-# SPECIALISE json :: Acceptor ByteString Value #-}

json_ :: CharParser StringType p => p Value -> p Value -> p Value
json_ obj ary = do
  w <- space *> (char '{' <|> char '[')
  if w == '{'
    then obj
    else ary
{-# INLINE json_ #-}
{-# SPECIALISE json_ :: Acceptor ByteString Value -> Acceptor ByteString Value -> Acceptor ByteString Value #-}

object_ :: Parser Value
object_ = Object <$> objectValues jstring value
{-# SPECIALISE object_ :: Acceptor ByteString Value #-}

objectValues :: CharParser StringType p => p Text -> p Value -> p (H.HashMap Text Value)
objectValues str val = do
  space
  let pair = liftA2 (,) (str <* space) (char ':' *> space *> val)
  H.fromList <$> commaSeparated pair '}'
{-# INLINE objectValues #-}
{-# SPECIALISE objectValues :: Acceptor ByteString Text -> Acceptor ByteString Value -> Acceptor ByteString (H.HashMap Text Value) #-}

array_ :: Parser Value
array_ = Array <$> arrayValues value
{-# SPECIALISE array_ :: Acceptor ByteString Value #-}

arrayValues :: CharParser StringType p => p Value -> p (Vector Value)
arrayValues val = do
  space
  V.fromList <$> commaSeparated val ']'
{-# INLINE arrayValues #-}
{-# SPECIALISE arrayValues :: Acceptor ByteString Value -> Acceptor ByteString (Vector Value) #-}

commaSeparated :: CharParser StringType p => p a -> Char -> p [a]
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
{-# SPECIALISE commaSeparated :: Acceptor ByteString Value -> Char -> Acceptor ByteString [Value] #-}
{-# SPECIALISE commaSeparated :: Acceptor ByteString (Text, Value) -> Char -> Acceptor ByteString [(Text, Value)] #-}

value :: Parser Value
value = do
  w <- lookAhead anyChar
  case w of
    '"' -> anyChar *> (String <$> jstring_)
    '{' -> anyChar *> object_
    '[' -> anyChar *> array_
    'f' -> Bool False <$ chunk "false"
    't' -> Bool True  <$ chunk "true"
    'n' -> chunk "null" *> pure Null
    _
      | w >= '0' && w <= '9' || w == '-' -> Number <$> scientific
      | otherwise -> fail "not a valid json value"
{-# SPECIALISE value :: Acceptor ByteString Value #-}

jstring :: Parser Text
jstring = char '"' *> jstring_
{-# SPECIALISE jstring :: Acceptor ByteString Text #-}

jstring_ :: Parser Text
jstring_ =
  T.decodeUtf8 <$> asChunk (skipMany $ satisfy (/= '"')) <* char '"'
{-# INLINE jstring_ #-}
{-# SPECIALISE jstring_ :: Acceptor ByteString Text #-}

space :: Parser ()
space = skipMany (satisfy (\c -> c == ' ' || c == '\n' || c == '\t'))
{-# SPECIALISE space :: Acceptor ByteString () #-}

scientific :: Parser Sci.Scientific
scientific = do
  neg <- option id $ negate <$ char '-'
  (c, _, e) <- fractionDec (pure ())
  pure $ Sci.scientific (neg c) (fromIntegral e)
{-# SPECIALISE scientific :: Acceptor ByteString Sci.Scientific #-}
