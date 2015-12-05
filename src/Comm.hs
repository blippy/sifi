module Comm where

import Data.Either
import Data.List
import Data.Maybe
import Data.String.Utils

import Types
import Utils
import Yahoo






findComm :: [Comm] -> Sym -> Comm
findComm comms sym =
  case hit of
    Just value -> value
    Nothing -> error ("ERR: findComm couldn't find Comm with Sym " ++ sym)
  where
    hit = find (\c -> sym == (cmSym c)) comms

findTicker :: [Comm] -> Sym -> Ticker
findTicker comms sym = cmYepic $ findComm comms sym
               



  
yepics comms = map cmYepic $ filter cmFetch comms    

deriveComm :: Dstamp -> Dstamp -> [StockQuote] -> Comm -> Comm
deriveComm start end quotes comm =
  comm'
  where
    yepic = cmYepic comm
    startPrice = getStockQuote (\d -> d < start) yepic quotes
    endPrice = getStockQuote (\d -> d <= end) yepic quotes
    comm' = comm { cmStartPrice = startPrice, cmEndPrice = endPrice }
  
deriveComms start end quotes comms  =
  map (deriveComm start end quotes) comms


commStartPriceOrDie comms sym =
  doOrDie (cmStartPrice comm) ("Can't find start price for:'" ++ sym ++ "'")
  where
    comm = findComm comms sym
  

commEndPriceOrDie comms sym =
  doOrDie (cmEndPrice comm) ("Can't find end price for:" ++ sym)
  where
    comm = findComm comms sym


precacheCommsUsing :: Bool -> [Comm] -> IO [Either String StockQuote]
precacheCommsUsing concurrently comms = do
  quotes <- fetchCommQuotes concurrently comms -- will be filtered automatically
  file1 <- outFile "yahoo-cached.txt"
  saveStockQuotes file1 $ rights quotes
  ds <- dateString
  fname <- outFile ("yahoo" ++ fileSep ++ ds ++ ".txt")
  --let fname = "/home/mcarter/.ssa/yahoo/" ++ ds ++ ".txt"
  saveStockQuotes fname $rights quotes
  return quotes


rox :: Double -> Comm -> Double
rox  usd c =
  scale
  where
    curr = cmUnit c
    tbl = [ ("USD", usd), ("P", 1.0), ("GBP", 100.0), ("NIL", 1.0) ]
    lup = lookup curr tbl
    scale = case lup of
      Nothing -> 666.0
      Just q -> q

-- | Fetch StockQuotes of Comm for which a fetch is required
--fetchCommQuotes :: [Comm] ->  IO [StockQuote]
fetchCommQuotes concurrently comms = do
  let hitComms = filter cmFetch comms
  let tickers = map cmYepic hitComms
  usd <- fetchUsd
  let roxs = map (rox usd) hitComms
  fetchQuotesA concurrently tickers roxs
