{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Log.PariPari
  ( parseLog )
where

import Data.Time
import Data.Word (Word8)
import ParsersBench.Log.Common
import Text.PariPari

parseLog :: ByteString -> Log
parseLog bs =
  case runParser logParser "" bs of
    Left err -> error (show err)
    Right x -> x

parseIP :: Parser p => p IP
parseIP = do
  d1 <- decimal
  void (char '.')
  d2 <- decimal
  void (char '.')
  d3 <- decimal
  void (char '.')
  d4 <- decimal
  return (IP d1 d2 d3 d4)

timeParser :: Parser p => p LocalTime
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

productParser :: Parser p => p Product
productParser =
      (Mouse    <$ string "mouse")
  <|> (Keyboard <$ string "keyboard")
  <|> (Monitor  <$ string "monitor")
  <|> (Speakers <$ string "speakers")

logEntryParser :: Parser p => p LogEntry
logEntryParser = do
  t <- timeParser
  void (char ' ')
  ip <- parseIP
  void (char ' ')
  p <- productParser
  return (LogEntry t ip p)

logParser :: Parser p => p Log
logParser = many (logEntryParser <* char '\n')

decimal :: Parser p => p Word8
decimal = fst <$> integer (pure ()) 10
