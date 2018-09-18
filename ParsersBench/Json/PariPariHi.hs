{-# OPTIONS_GHC -F -pgmF paripari-specialise-all #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module ParsersBench.Json.PariPariHi
  ( parseJson, parseJsonReporter )
where

import Control.Applicative
import ParsersBench.Json.Common
import Text.PariPari
import qualified Data.HashMap.Strict as H
import qualified Data.Vector         as V
import qualified Data.Scientific     as Sci
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding  as T

type StringType    = Text
type ParserMonad p = CharParser StringType p
type Parser a      = (forall p. ParserMonad p => p a)

{-# SPECIALISE_ALL ParserMonad p = p ~ Acceptor StringType #-}
{-# SPECIALISE_ALL ParserMonad p = p ~ Reporter StringType #-}
{-# SPECIALISE_ALL Parser = Acceptor StringType #-}
{-# SPECIALISE_ALL Parser = Reporter StringType #-}

parseJson :: ByteString -> Value
parseJson bs =
  case runCharParser json "" (T.decodeUtf8 bs) of
    Left err -> error (show err)
    Right x -> x

parseJsonReporter :: ByteString -> Value
parseJsonReporter bs =
  case runReporter json "" (T.decodeUtf8 bs) of
    Left err -> error (show err)
    Right x -> x

json :: Parser Value
json = space *> (object <|> array)

object :: Parser Value
object = Object . H.fromList <$> (char '{' *> space *> sepBy pair (space *> char ',' *> space) <* space <* char '}')
  where pair = liftA2 (,) (jstring <* space) (char ':' *> space *> value)

array :: Parser Value
array = Array . V.fromList <$> (char '[' *> sepBy value (space *> char ',' *> space) <* space <* char ']')

value :: Parser Value
value = do
  (String <$> jstring)
    <|> object
    <|> array
    <|> (Bool False <$ chunk "false")
    <|> (Bool True  <$ chunk "true")
    <|> (Null       <$ chunk "null")
    <|> (Number     <$> scientific)

jstring :: Parser Text
jstring = char '"' *> asChunk (skipMany $ satisfy (/= '"')) <* char '"'

space :: Parser ()
space = skipMany (satisfy (\c -> c == ' ' || c == '\n' || c == '\t'))

scientific :: Parser Sci.Scientific
scientific = do
  neg <- sign
  frac <- fractionDec (pure ())
  pure $ case frac of
           Left n -> Sci.scientific (neg n) 0
           Right (c, _, e) -> Sci.scientific (neg c) (fromIntegral e)
