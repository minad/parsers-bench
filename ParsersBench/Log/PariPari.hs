{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module ParsersBench.Log.PariPari
  ( parseLog )
where

import Data.Time
import ParsersBench.Log.Common
import Text.PariPari
import Data.ByteString (ByteString)

type StringType = ByteString
type Parser a = (forall p. CharParser StringType p => p a)

parseLog :: ByteString -> Log
parseLog bs =
  case runCharParser logParser "" bs of
    Left err -> error (show err)
    Right x -> x

parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  void (char '.')
  d2 <- decimal
  void (char '.')
  d3 <- decimal
  void (char '.')
  d4 <- decimal
  return (IP d1 d2 d3 d4)

timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 $ digitChar 10
  void (char '-')
  mm <- count 2 $ digitChar 10
  void (char '-')
  d  <- count 2 $ digitChar 10
  void (char ' ')
  h  <- count 2 $ digitChar 10
  void (char ':')
  m  <- count 2 $ digitChar 10
  void (char ':')
  s  <- count 2 $ digitChar 10
  return LocalTime
    { localDay       = fromGregorian (read y) (read mm) (read d)
    , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
    }

productParser :: Parser Product
productParser =
      (Mouse    <$ chunk "mouse")
  <|> (Keyboard <$ chunk "keyboard")
  <|> (Monitor  <$ chunk "monitor")
  <|> (Speakers <$ chunk "speakers")

logEntryParser :: Parser LogEntry
logEntryParser = do
  t <- timeParser
  void (char ' ')
  ip <- parseIP
  void (char ' ')
  p <- productParser
  return (LogEntry t ip p)

logParser :: Parser Log
logParser = many (logEntryParser <* char '\n')
