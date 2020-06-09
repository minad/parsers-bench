{-# OPTIONS_GHC -F -pgmF paripari-specialise-all #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module ParsersBench.Json.PariPari
  ( parseJson, parseJsonReporter )
where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import ParsersBench.Json.Common
import Text.PariPari
import qualified Data.HashMap.Strict as H
import qualified Data.Scientific     as Sci
import qualified Data.Text.Encoding  as T
import qualified Data.Vector         as V

type StringType    = ByteString
type PMonad p = Parser StringType p
type P a      = (forall p. PMonad p => p a)

{-# SPECIALISE_ALL PMonad p = p ~ Acceptor StringType #-}
{-# SPECIALISE_ALL PMonad p = p ~ Reporter StringType #-}
{-# SPECIALISE_ALL P = Acceptor StringType #-}
{-# SPECIALISE_ALL P = Reporter StringType #-}

parseJson :: ByteString -> Value
parseJson bs =
  case runParser json "" bs of
    (Just x, []) -> x
    (_, err) -> error (show err)

parseJsonReporter :: ByteString -> Value
parseJsonReporter bs =
  case runReporter json "" bs of
    (Just x, []) -> x
    (_, err) -> error (show err)

json :: P Value
json = json_ object_ array_

json_ :: PMonad p => p Value -> p Value -> p Value
json_ obj ary = do
  w <- space *> (char '{' <|> char '[')
  if w == '{'
    then obj
    else ary
{-# INLINE json_ #-}

object_ :: P Value
object_ = Object <$> objectValues jstring value

objectValues :: PMonad p => p Text -> p Value -> p (H.HashMap Text Value)
objectValues str val = do
  space
  let pair = liftA2 (,) (str <* space) (char ':' *> space *> val)
  H.fromList <$> commaSeparated pair '}'
{-# INLINE objectValues #-}

array_ :: P Value
array_ = Array <$> arrayValues value

arrayValues :: PMonad p => p Value -> p (Vector Value)
arrayValues val = do
  space
  V.fromList <$> commaSeparated val ']'
{-# INLINE arrayValues #-}

commaSeparated :: PMonad p => p a -> Char -> p [a]
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

value :: P Value
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

jstring :: P Text
jstring = char '"' *> jstring_

jstring_ :: P Text
jstring_ =
  T.decodeUtf8 <$> asChunk (skipMany $ satisfy (/= '"')) <* char '"'
{-# INLINE jstring_ #-}

space :: P ()
space = skipMany (satisfy (\c -> c == ' ' || c == '\n' || c == '\t'))

scientific :: P Sci.Scientific
scientific = do
  neg <- sign
  frac <- fractionDec (pure ())
  pure $ case frac of
           Left n -> Sci.scientific (neg n) 0
           Right (c, _, e) -> Sci.scientific (neg c) (fromIntegral e)
