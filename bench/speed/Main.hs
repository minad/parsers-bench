module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString                as B
import qualified ParsersBench.CSV.Attoparsec    as A
import qualified ParsersBench.CSV.Megaparsec    as M
import qualified ParsersBench.CSV.PariPari     as P
import qualified ParsersBench.Json.Attoparsec   as A
import qualified ParsersBench.Json.Megaparsec   as M
import qualified ParsersBench.Json.PariPari    as P
import qualified ParsersBench.Json.MegaparsecHi as MH
import qualified ParsersBench.Json.PariPariHi  as PH
import qualified ParsersBench.Json.AttoparsecHi as AH
import qualified ParsersBench.Log.Attoparsec    as A
import qualified ParsersBench.Log.Megaparsec    as M
import qualified ParsersBench.Log.PariPari     as P

main :: IO ()
main = defaultMain
  [ bgroup "CSV (PariPari)"
    [ bparser file P.parseCSV | file <- csvFiles ]
  , bgroup "CSV (PariPari, Reporter)"
    [ bparser file P.parseCSVReporter | file <- csvFiles ]
  , bgroup "CSV (Attoparsec)"
    [ bparser file A.parseCSV | file <- csvFiles ]
  , bgroup "CSV (Megaparsec)"
    [ bparser file M.parseCSV | file <- csvFiles ]
  , bgroup "Log (PariPari)"
    [ bparser file P.parseLog | file <- logFiles ]
  , bgroup "Log (PariPari, Reporter)"
    [ bparser file P.parseLogReporter | file <- logFiles ]
  , bgroup "Log (Attoparsec)"
    [ bparser file A.parseLog | file <- logFiles ]
  , bgroup "Log (Megaparsec)"
    [ bparser file M.parseLog | file <- logFiles ]
  , bgroup "JSON (PariPari)"
    [ bparser file P.parseJson | file <- jsonFiles ]
  , bgroup "JSON (PariPari, Reporter)"
    [ bparser file P.parseJsonReporter | file <- jsonFiles ]
  , bgroup "JSON (Attoparsec)"
    [ bparser file A.parseJson | file <- jsonFiles ]
  , bgroup "JSON (Megaparsec)"
    [ bparser file M.parseJson | file <- jsonFiles ]
  , bgroup "JSON (PariPari, highlevel)"
    [ bparser file PH.parseJson | file <- jsonFiles ]
  , bgroup "JSON (PariPari, Reporter, highlevel)"
    [ bparser file PH.parseJsonReporter | file <- jsonFiles ]
  , bgroup "JSON (Attoparsec, highlevel)"
    [ bparser file AH.parseJson | file <- jsonFiles ]
  , bgroup "JSON (Megaparsec, highlevel)"
    [ bparser file MH.parseJson | file <- jsonFiles ]
  ]

bparser :: NFData a => FilePath -> (ByteString -> a) -> Benchmark
bparser desc f = env (B.readFile path) (bench desc . nf f)
  where
    path = "bench-data/" ++ desc

csvFiles :: [FilePath]
csvFiles =
  [-- "csv-5.csv"
  --, "csv-10.csv"
  --, "csv-20.csv"
   "csv-40.csv" ]

logFiles :: [FilePath]
logFiles =
  [-- "log-5.log"
  --, "log-10.log"
  --, "log-20.log"
   "log-40.log" ]

jsonFiles :: [FilePath]
jsonFiles =
  [-- "json-5.json"
  --, "json-10.json"
  --, "json-20.json"
   "json-40.json" ]
