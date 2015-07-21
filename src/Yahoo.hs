module Yahoo where

import Control.Concurrent.Async
import Control.Monad
--import Data.Char
import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils
import Data.Tuple.Select
import GHC.Exts
import Network.HTTP
import System.IO
import Text.Printf


-- import ConfigParser
import Types
import Utils


splitStr = Data.String.Utils.split -- alias a function



symUrl sym =
  pre ++ sym ++ post
  where
    pre =  "http://download.finance.yahoo.com/d/quotes.csv?s="
    post = "&f=sl1c1p2&e=.csv"

--type FetchedStr = String -- a string result fetched from Yahoo

getUrl :: String -> IO String
getUrl url = do
    response <- simpleHTTP $ getRequest url
    getResponseBody response

stripJunk = stripChars "\"+%\n"



fmtPrice :: Double -> String
fmtPrice p = printf "%10.4f" p


str4 :: Double -> String -- TODO promote to Utils
str4 d = printf "%9.4f" d

fmtTicker ticker = printf "%6s" ticker

quoteAsText :: StockQuote -> String
quoteAsText sq = 
  intercalate "  " fields
  where
    --fmt = "yahoo\t%s\t%s\t%s\t%12.4f\tP\n"
    fields = ["yahoo", dstamp, tstamp, (fmtTicker ticker), (str4 rox),   (str4 price), (str4 chg),   (str4 chgpc), "P\n"]
    StockQuote dstamp tstamp ticker rox price chg chgpc = sq
    

usdUrl = -- url for USD
  url1 ++ url2 ++ url3
  where
    url1 = "http://download.finance.yahoo.com/d/quotes.csv?s="
    url2 = "USDGBP"
    url3 = "=X&f=nl1d1t1"


fetchUsd = do
  resp <- getUrl usdUrl
  let clean = stripJunk resp
  let fields = splitStr "," clean
  --let roxStr = (fields !! 1)
  let roxf =
        case asEitherDouble (fields !! 1)  of
          Left err -> error $ unlines ["fetchUsd fail", "url:", usdUrl,
                                     "Response:", resp,
                                     "ROX Strings (needs 2nd item):", show fields,
                                     "Reporting Error:", err]
          Right v -> v
  let rox = 100.0 * roxf
  return rox

testQuoteAsText = do
  let sq = StockQuote "2015-03-18" "12:59:23" "HYH" 1.0 47.5745 0.5645 1.2008
  print (quoteAsText sq)


decodeFetch dstamp tstamp rox serverText ticker =
  ans
  where
    fields = splitStr "," serverText
    [_, priceStr, chgStr, chgpcStr] = fields
    mrox str = liftM2 (*) (Just rox) (asMaybeDouble str)
    price = mrox priceStr
    chg  = mrox chgStr
    chgpc = asMaybeDouble chgpcStr
    success = (length fields == 4) && all isJust [price, chg, chgpc]
    ok = Right $ StockQuote dstamp tstamp ticker rox  (fromJust price) (fromJust chg) (fromJust chgpc)
    fail = Left $ "Couldn't decipher server response for ticker '" ++ ticker++ "' with response '" ++ serverText ++ "'"
    ans = if success then ok else fail


-- fetchQuote ::  String -> String -> (String, Float)  -> IO StockQuote
fetchQuote dstamp tstamp (ticker, rox) =  do
  let url = symUrl $ replace "^" "%5E" ticker
  resp <- getUrl url
  let clean = stripJunk resp
  let sq =  decodeFetch dstamp tstamp rox clean ticker
  return sq


testFtas = fetchQuote  "2015-03-18" "12:59:23" ("^FTAS", 1.0)
testHyh = fetchQuote "2015-03-18" "12:59:23" ("HYH", 1.0)


--fetchQuotes :: Dstamp -> Tstamp -> [(Ticker, Rox)] -> IO [StockQuote]
fetchQuotes concurrently dstamp tstamp pairs = do
  let fq = fetchQuote dstamp tstamp
  let mapper = if concurrently then mapConcurrently else mapM
  res <- mapper fq pairs
  --let oops = lefts res
  --let _ = if length oops >0 then error (show oops) else []
  return res

fetchQuotesA :: Bool -> [[Char]] -> [Double] -> IO [Either [Char] StockQuote]
fetchQuotesA concurrently tickers roxs = do
  ds <- dateString
  ts <- timeString
  let pairs = zip tickers roxs
  quotes <- fetchQuotes concurrently ds ts pairs
  return quotes

testFqa = fetchQuotesA True ["HYH", "^FTAS"] [1.0, 1.0]
testFqaBad = fetchQuotesA True ["AARGH", "^FTAS"] [1.0, 1.0]

testTickers = [ ("AML.L", 1.0), ("ULVR.L", 1.0), ("HYH", 1000.0)]
testFetches = fetchQuotes True "2015-03-18" "12:59:23" testTickers

yfile = outFile "yahoo-cached.txt"

saveStockQuotes :: FilePath -> [StockQuote] -> IO ()
saveStockQuotes fname quotes = do
  h <- openFile fname WriteMode
  let writeQuote quote = hPrintf h (quoteAsText quote)
  mapM_ writeQuote quotes
  hFlush h
  hClose h


fetchAndSave :: Bool -> [(Ticker, Rox)] -> IO ()   
fetchAndSave concurrently tickerPairs = do
  dstamp <- dateString
  tstamp <- timeString
  let fname = "/home/mcarter/.ssa/yahoo/" ++ dstamp ++ ".txt"
  quotes <- fetchQuotes  concurrently dstamp tstamp tickerPairs
  let errs = lefts quotes
  if length errs >0 then print errs else print ""
  saveStockQuotes fname $ rights quotes 


testFas = fetchAndSave True testTickers



loadSaves = do
  yf <- yfile
  let txt = readFile yf
  let ls = (liftM lines) txt
  fmap (liftM decodeFetch) ls

-- | Return the a price for Ticker on or before Dstamp
getStockQuote :: (Dstamp -> Bool) -> Ticker -> [StockQuote] -> Maybe Double
getStockQuote accept ticker sqs =
  sqLast
  where
    f sq = ticker == (sqTicker sq) && accept (sqDstamp sq)
    matches = filter f sqs
    sortedMatches = sortWith sqDstamp matches
    sq [] = Nothing
    sq xs = Just $ sqPrice $ last xs
    sqLast = sq sortedMatches





testya = yahooEpics ["ulvr.l", "hyh", "azn.l"]
testya1 = yahooEpics ["ulvr.l", "yikes"]

fmtSqBrief sq =
  intercalate " " fields
  where
    StockQuote _ _ ticker _ price chg chgpc = sq
    fields = [fmtTicker ticker, fmtPrice price, str4 chg, str4 chgpc]

yahooEpics :: [String] -> IO ()
yahooEpics epics = do
  --        "ULVR.L  2787.0000   -1.0000   -0.0359"
  let hdr = "EPIC        PRICE       CHG      CHG%"
  putStrLn hdr
  let epics1 = epics ++ ["^FTSE", "^FTAS", "^FTMC"]
  let epics2 = map upperCase epics1

  let prin sq = case sq of
        Left x -> x
        Right x -> fmtSqBrief x
        
  rawStockQuotes <- fetchQuotesA True epics2 ones
  let rows = map prin rawStockQuotes
  putAll rows
