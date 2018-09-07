{-# OPTIONS_GHC -F -pgmF paripari-specialise-all #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module ParsersBench.CSV.PariPari
  ( parseCSV )
where

import Control.Monad ((<$!>))
import Data.Foldable (toList)
import Data.Vector (Vector)
import Data.List.NonEmpty (NonEmpty)
import Text.PariPari
import qualified Data.Vector as V
import Data.ByteString (ByteString)

type Record        = Vector StringType
type StringType    = ByteString
type ParserMonad p = CharParser StringType p
type Parser a      = (forall p. ParserMonad p => p a)

{-# SPECIALISE_ALL ParserMonad p = p ~ Acceptor StringType #-}
{-# SPECIALISE_ALL ParserMonad p = p ~ Reporter StringType #-}
{-# SPECIALISE_ALL Parser = Acceptor StringType #-}
{-# SPECIALISE_ALL Parser = Reporter StringType #-}

-- | Parse a CSV file without conversion of individual records.

parseCSV :: ByteString -> NonEmpty Record
parseCSV bs =
  case runCharParser csv "" bs of
    Left err -> error (show err)
    Right x -> x

csv :: Parser (NonEmpty Record)
csv = do
  xs <- sepEndBy1 record (char '\n')
  eof
  return xs

record :: Parser Record
record = do
  notFollowedBy eof -- to prevent reading empty line at the end of file
  V.fromList . toList <$!> (sepBy1 field (char ',') <?> "record")

field :: Parser StringType
field = label "field" (escapedField <|> unescapedField)

escapedField :: Parser StringType
escapedField =
  between (char '"') (char '"') (asChunk $ skipMany $ normalChar <|> escapedDq)
  where
    normalChar = notChar '"' <?> "unescaped character"
    escapedDq  = label "escaped double-quote" ('"' <$ chunk "\"\"")

unescapedField :: Parser StringType
unescapedField = asChunk $ skipMany $ satisfy (\c -> c /= ',' && c /= '"' && c /= '\n' && c /= '\r')
{-# INLINE unescapedField #-}
