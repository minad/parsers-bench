{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Rank2Types        #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Semigroup ((<>))
import Data.Text (Text)
import Text.PariPari
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

main :: IO ()
main = defaultMain
  [ bparser "string"   manyAs (string . fst)
  , bparser "string'"  manyAs (string' . fst)
  , bparser "many"     manyAs (const $ many (char 'a'))
  , bparser "some"     manyAs (const $ some (char 'a'))
  , bparser "choice"   (const "b") (choice . fmap char . manyAsB' . snd)
  , bparser "count"    manyAs (\(_,n) -> count n (char 'a'))
  , bparser "count'"   manyAs (\(_,n) -> count' 1 n (char 'a'))
  , bparser "endBy"    manyAbs' (const $ endBy (char 'a') (char 'b'))
  , bparser "endBy1"   manyAbs' (const $ endBy1 (char 'a') (char 'b'))
  , bparser "manyTill" manyAsB (const $ manyTill (char 'a') (char 'b'))
  , bparser "someTill" manyAsB (const $ someTill (char 'a') (char 'b'))
  , bparser "sepBy"    manyAbs (const $ sepBy (char 'a') (char 'b'))
  , bparser "sepBy1"   manyAbs (const $ sepBy1 (char 'a') (char 'b'))
  , bparser "sepEndBy"  manyAbs' (const $ sepEndBy (char 'a') (char 'b'))
  , bparser "sepEndBy1" manyAbs' (const $ sepEndBy1 (char 'a') (char 'b'))
  , bparser "skipMany" manyAs (const $ skipMany (char 'a'))
  , bparser "skipSome" manyAs (const $ skipSome (char 'a'))
  , bparser "skipCount" manyAs (\(_,n) -> skipCount n (char 'a'))
  , bparser "skipManyTill" manyAsB (const $ skipManyTill (char 'a') (char 'b'))
  , bparser "skipSomeTill" manyAsB (const $ skipSomeTill (char 'a') (char 'b'))
  , bparser "takeWhileP" manyAs (const $ takeCharsWhile (== 'a'))
  , bparser "takeWhile1P" manyAs (const $ takeCharsWhile1 (== 'a'))
  , bparser "skipCharsWhile" manyAs (const $ skipCharsWhile (== 'a'))
  , bparser "skipCharsWhile1" manyAs (const $ skipCharsWhile (== 'a'))
  , bparser "decimal" mkInt (const (decimal :: Acceptor Integer))
  , bparser "octal" mkInt (const (octal :: Acceptor Integer))
  , bparser "hexadecimal" mkInt (const (hexadecimal :: Acceptor Integer))
  , bparser "scientific" mkInt (const (fractionDec (pure ()) :: Acceptor (Integer, Int, Integer)))
  , bparser "takeBytesWhile" manyAs (const $ takeBytesWhile (== 97))
  , bparser "takeBytesWhile1" manyAs (const $ takeBytesWhile (== 97))
  , bparser "skipBytesWhile" manyAs (const $ skipBytesWhile (== 97))
  , bparser "skipBytesWhile1" manyAs (const $ skipBytesWhile (== 97))
  ]

instance NFData Error

-- | Perform a series to measurements with the same parser.

bparser :: NFData a
  => String            -- ^ Name of the benchmark group
  -> (Int -> Text)     -- ^ How to construct input
  -> ((Text, Int) -> Acceptor a) -- ^ The parser receiving its future input
  -> Benchmark         -- ^ The benchmark
bparser name f p = bgroup name (bs <$> stdSeries)
  where
    bs n = env (return (f n, n)) (bench (show n) . nf p')
    p' (s,n) = runAcceptor (p (s,n)) "" (T.encodeUtf8 s)

-- | The series of sizes to try as part of 'bparser'.

stdSeries :: [Int]
stdSeries = [500,1000,2000,4000]

----------------------------------------------------------------------------
-- Helpers

-- | Generate that many \'a\' characters.

manyAs :: Int -> Text
manyAs n = T.replicate n "a"

-- | Like 'manyAs', but interspersed with \'b\'s.

manyAbs :: Int -> Text
manyAbs n = T.take (if even n then n + 1 else n) (T.replicate n "ab")

-- | Like 'manyAs', but with a \'b\' added to the end.

manyAsB :: Int -> Text
manyAsB n = manyAs n <> "b"

-- | Like 'manyAsB', but returns a 'String'.

manyAsB' :: Int -> String
manyAsB' n = replicate n 'a' ++ "b"

-- | Like 'manyAbs', but ends in a \'b\'.

manyAbs' :: Int -> Text
manyAbs' n = T.take (if even n then n else n + 1) (T.replicate n "ab")

-- | Render an 'Integer' with the number of digits linearly dependent on the
-- argument.

mkInt :: Int -> Text
mkInt n = (T.pack . show) ((10 :: Integer) ^ (n `quot` 100))
