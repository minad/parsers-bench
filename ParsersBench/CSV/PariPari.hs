{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module ParsersBench.CSV.PariPari
  ( parseCSV )
where

import Control.Monad ((<$!>))
import Data.Foldable (toList)
import Data.Vector (Vector)
import Text.PariPari
import qualified Data.Vector as V

type Record = Vector Field
type Field  = Text

-- | Parse a CSV file without conversion of individual records.

parseCSV :: ByteString -> NonEmpty Record
parseCSV bs =
  case runParser csv "" bs of
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

field :: Parser Field
field = label "field" (escapedField <|> unescapedField)

escapedField :: Parser Text
escapedField =
  between (char '"') (char '"') (asString $ skipMany $ normalChar <|> escapedDq)
  where
    normalChar = notChar '"' <?> "unescaped character"
    escapedDq  = label "escaped double-quote" ('"' <$ string "\"\"")

unescapedField :: Parser Text
unescapedField = asString $ skipMany $ satisfy (\c -> c /= ',' && c /= '"' && c /= '\n' && c /= '\r')
{-# INLINE unescapedField #-}
