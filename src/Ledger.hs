module Ledger  where

import Control.Monad
import Data.Either

import Comm
import Dps
import Etran
import Financial
import Nacc
--import Ntran
import Parser
import Returns
import Types
import Utils
import Yahoo

data StockTrip = StockTrip
                 { stFile :: [StockQuote] -- from file cache
                 , stSynth :: [StockQuote] -- synthesised stock quotes
                 , stWeb :: [StockQuote] -- stock quotes downloaded from web
                 } deriving Show

allSt:: StockTrip -> [StockQuote]
allSt (StockTrip f s w) = f ++ s ++ w
                 
data Ledger = Ledger -- FIXME should derive from Records
    {
      ldRecords :: Records
--      comms :: [Comm]
--    , dpss :: [Dps]
--    , etrans :: [Etran]
--    , financials :: [Financial]
--    , ntrans :: [Ntran]
--    , naccs :: [Nacc]
    , start :: Dstamp
    , end :: Dstamp
    , squotes :: StockTrip
--    , returns :: [Return]
--    , xaccs :: [Xacc]
    } deriving Show

comms = rcComms . ldRecords
dpss  = rcDpss . ldRecords
etrans = rcEtrans . ldRecords
financials = rcFinancials . ldRecords
ntrans = rcNtrans . ldRecords
naccs = rcNaccs . ldRecords
returns = rcReturns . ldRecords
xaccs = rcXaccs . ldRecords

ledgerQuotes ledger = allSt $ squotes ledger


trimLedger :: Ledger -> Ledger
trimLedger ledger =
  ledger { ldRecords = recs' }
  where
    etrans' = filter (\e -> (etDstamp e) <= (end ledger)) $ etrans ledger

    trNtrans :: [Ntran] -> [Ntran] -> [Ntran]
    trNtrans acc ([]) = reverse acc
    trNtrans acc (n:ns) =
      if ntDstamp n > end ledger
      then trNtrans acc ns
      else trNtrans (n':acc) ns
      where
        Ntran dstamp dr cr pennies clear desc = n
        theNaccs = naccs ledger
        (dr', cr') = if dstamp < (start ledger)
                     then (alt dr theNaccs, alt cr theNaccs)
                     else (dr, cr)
        n' = Ntran dstamp dr' cr' pennies clear desc
    ntrans' = trNtrans [] $ ntrans ledger

    comms' = deriveComms (start ledger) (end ledger) (ledgerQuotes ledger) (comms ledger)
    etrans'' = deriveEtrans (start ledger) comms' etrans'

    recs = ldRecords ledger
    --recs1 = recs { rcComms = (rcComms recs ++ comms') }
    --recs2 = recs1 { rcEtrans = (rcEtrans recs ++ etrans'') }
    --recs3 = recs2 { rcNtrans = (rcNtrans recs ++ ntrans') }
    recs' = recs { rcComms = comms', rcEtrans = etrans'', rcNtrans = ntrans' }


    --trNtrans = filter (\n -> (ntranDstamp n) <= (end ledger)) $ ntrans ledger







{-
readLedger' inputs =
  Ledger { comms = comms
         , dpss = getDpss inputs
         , etrans = etrans
         , financials = getFinancials inputs
         , ntrans = getNtrans inputs
         , naccs = getNaccs inputs
         , start = start
         , end = end
         , squotes = quotes
         , returns = getReturns inputs
         , xaccs = getXaccs inputs
         }
  where
    comms = getComms inputs
    etrans = getEtrans inputs
    (start, end) = last $ getPeriods inputs
    yahoos = getQuotes inputs 
    googles = getGoogles inputs
    synths = synthSQuotes comms etrans
    quotes = StockTrip (yahoos ++ googles)  synths []
-}

readLedger' :: Records -> Ledger
readLedger' recs =
  Ledger {
    ldRecords = recs
    , start = start
    , end = end
    , squotes = quotes
    }
  where
    --comms = getComms inputs
    --etrans = getEtrans inputs
    (start, end) = last (rcPeriods recs)
    --yahoos = getQuotes inputs 
    --googles = getGoogles inputs
    synths = synthSQuotes (rcComms recs) (rcEtrans recs)
    quotes = StockTrip (rcQuotes recs)  synths []


freshQuotes :: Ledger -> Bool -> IO [Either String StockQuote]
freshQuotes ledger downloading = 
  if downloading then precacheCommsUsing True (comms ledger) else return ([])


ratl' = do
  recs <- radi
  return $ readLedger' recs

-- | Read and trim ledger
ratl :: Bool -> IO Ledger
--ratlXXX = liftM trimLedger readLedger -- FIXME NOW do downloading if necessary
ratl fetch = do
  --inputs <- readInputs

  --recs <- radi
  --let ledger1 = readLedger' recs
  ledger1 <- ratl'
  let squotes1 = squotes ledger1
  (errs, quotes) <- fmap partitionEithers $ freshQuotes ledger1 fetch -- FIXME handle errs
  let squotes2 = squotes1 { stWeb = quotes }
  let ledger2 = ledger1 { squotes = squotes2 }
  
  let ledger3 = trimLedger ledger2
  return ledger3

ratlf = ratl False

etranToSQuote :: [Comm] -> Etran -> StockQuote
etranToSQuote comms e =
  StockQuote ds "08:00:00" ticker 1.0 price 0.0 0.0
  where
    ds = etDstamp e
    ticker = findTicker comms (etSym e)
    amount = unPennies $ etAmount e
    qty = etQty e
    price = 100.0 * amount / qty

            
synthSQuotes :: [Comm] -> [Etran] -> [StockQuote] -- create synthetic stock quotes
synthSQuotes comms etrans =  map  (etranToSQuote comms)  etrans


