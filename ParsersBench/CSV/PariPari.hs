{-# OPTIONS_GHC -F -pgmF paripari-specialise-all #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module ParsersBench.CSV.PariPari
  ( parseCSV, parseCSVReporter )
where

import Control.Monad ((<$!>))
import Data.Foldable (toList)
import Data.Vector (Vector)
import Data.List.NonEmpty (NonEmpty)
import Text.PariPari
import qualified Data.Vector as V
import Data.ByteString (ByteString)

type Record     = Vector StringType
type StringType = ByteString
type PMonad p   = Parser StringType p
type P a        = (forall p. PMonad p => p a)

{-# SPECIALISE_ALL PMonad p = p ~ Acceptor StringType #-}
{-# SPECIALISE_ALL PMonad p = p ~ Reporter StringType #-}
{-# SPECIALISE_ALL P = Acceptor StringType #-}
{-# SPECIALISE_ALL P = Reporter StringType #-}

-- | Parse a CSV file without conversion of individual records.

parseCSV :: ByteString -> NonEmpty Record
parseCSV bs =
  case runParser csv "" bs of
    (Just x, []) -> x
    (_, err) -> error (show err)

parseCSVReporter :: ByteString -> NonEmpty Record
parseCSVReporter bs =
  case runReporter csv "" bs of
    (Just x, []) -> x
    (_, err) -> error (show err)

csv :: P (NonEmpty Record)
csv = do
  xs <- sepEndBy1 record (char '\n')
  eof
  return xs

record :: P Record
record = do
  notFollowedBy eof -- to prevent reading empty line at the end of file
  V.fromList . toList <$!> (sepBy1 field (char ',') <?> "record")

field :: P StringType
field = label "field" (escapedField <|> unescapedField)

escapedField :: P StringType
escapedField =
  between (char '"') (char '"') (asChunk $ skipMany $ normalChar <|> escapedDq)
  where
    normalChar = notChar '"' <?> "unescaped character"
    escapedDq  = label "escaped double-quote" ('"' <$ chunk "\"\"")

unescapedField :: P StringType
unescapedField = asChunk $ skipMany $ satisfy (\c -> c /= ',' && c /= '"' && c /= '\n' && c /= '\r')
{-# INLINE unescapedField #-}
