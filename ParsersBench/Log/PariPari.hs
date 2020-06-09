{-# OPTIONS_GHC -F -pgmF paripari-specialise-all #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module ParsersBench.Log.PariPari
  ( parseLog, parseLogReporter )
where

import Data.Time
import ParsersBench.Log.Common
import Text.PariPari
import Data.ByteString (ByteString)

type StringType    = ByteString
type PMonad p = Parser StringType p
type P a      = (forall p. PMonad p => p a)

{-# SPECIALISE_ALL PMonad p = p ~ Acceptor StringType #-}
{-# SPECIALISE_ALL PMonad p = p ~ Reporter StringType #-}
{-# SPECIALISE_ALL P = Acceptor StringType #-}
{-# SPECIALISE_ALL P = Reporter StringType #-}

parseLog :: ByteString -> Log
parseLog bs =
  case runParser logParser "" bs of
    (Just x, []) -> x
    (_, err) -> error (show err)

parseLogReporter :: ByteString -> Log
parseLogReporter bs =
  case runReporter logParser "" bs of
    (Just x, []) -> x
    (_, err) -> error (show err)

parseIP :: P IP
parseIP = do
  d1 <- decimal
  void (char '.')
  d2 <- decimal
  void (char '.')
  d3 <- decimal
  void (char '.')
  d4 <- decimal
  return (IP d1 d2 d3 d4)

timeParser :: P LocalTime
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

productParser :: P Product
productParser =
      (Mouse    <$ chunk "mouse")
  <|> (Keyboard <$ chunk "keyboard")
  <|> (Monitor  <$ chunk "monitor")
  <|> (Speakers <$ chunk "speakers")

logEntryParser :: P LogEntry
logEntryParser = do
  t <- timeParser
  void (char ' ')
  ip <- parseIP
  void (char ' ')
  p <- productParser
  return (LogEntry t ip p)

logParser :: P Log
logParser = many (logEntryParser <* char '\n')
