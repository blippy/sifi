module Ledger  where

import Control.Monad
import Data.Either
import Safe (lastDef)

import Args
import Comm
import Dps
import Etran
import Financial
import Nacc
import Parser
import Returns
import Types
import Utils
import Yahoo



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
    recs' = recs { rcComms = comms', rcEtrans = etrans'', rcNtrans = ntrans' }




readLedger' :: Records -> Options -> Ledger
readLedger' recs opts =
  Ledger {
    ldRecords = recs
    , start = start
    , end = end
    , squotes = quotes
    }
  where
    pers = rcPeriods recs
    
    (defStart, defEnd) = lastDef ("0000-00-00", "3000-12-31") pers
    -- FIXME following could be written as one-liners
    start = case (optStart opts) of
      Just x -> x
      Nothing -> defStart
    end = case (optEnd opts) of
      Just x -> x
      Nothing -> defEnd
      
    synths = synthSQuotes (rcComms recs) (rcEtrans recs)
    quotes = StockTrip (rcQuotes recs)  synths []


freshQuotes :: Ledger -> Bool -> IO [Either String StockQuote]
freshQuotes ledger downloading = 
  if downloading then precacheCommsUsing True (comms ledger) else return ([])


ratl' cmdOptions = do
  --print (optEnd cmdOptions) -- FIXME remove
  recs <- radi
  return $ readLedger' recs cmdOptions

-- | Read and trim ledger
ratl :: Bool -> Options -> IO Ledger
ratl fetch cmdOptions = do
  --print (optEnd cmdOptions) -- FIXME remove
  ledger1 <- ratl' cmdOptions
  let squotes1 = squotes ledger1
  (errs, quotes) <- fmap partitionEithers $ freshQuotes ledger1 fetch -- FIXME handle errs
  let squotes2 = squotes1 { stWeb = quotes }
  let ledger2 = ledger1 { squotes = squotes2 }
  
  let ledger3 = trimLedger ledger2
  return ledger3

ratlf = ratl False defaultOptions

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


