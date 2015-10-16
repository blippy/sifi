module Snap  where

import Data.Either
import Data.Function
import Data.List
import Data.Ord
import Data.Tuple.Select
import GHC.Exts
import Text.Printf

--import Aggregate
import Comm
import Etran
import Ledger
import Types
import Utils
import Yahoo


-- FIXME LOW Handle cases of etrans not in comms

totalQty ::  [Etran] -> Comm -> Qty
totalQty etrans comm =
  qtys commEtrans
  where
    hit e = (cmSym comm) == (etSym e)
    commEtrans = filter hit  etrans


snapFmt = "%8s %9.2f %8.2f %6.2f"

mkSnapLine :: (StockQuote, Qty) -> (String, Double, Double)
mkSnapLine (sq, qty) =
  (str, amount, chg1)
  where
    StockQuote _ _ ticker _ price chg chgpc = sq
    amount = price * qty / 100.0
    chg1 = chg * qty / 100.0
    str = printf snapFmt ticker amount chg1 chgpc    



-- | False => use cached version, True => download values afresh
snapDownloading :: [Comm] -> Bool -> Bool -> IO [StockQuote]
snapDownloading theComms concurrently afresh = do
  pres <- fmap partitionEithers $ precacheCommsUsing concurrently theComms
  loaded <- loadPrecachedComms
  let (errs, fetchedQuotes) = if afresh
                              then  pres
                              else ([], loaded)
  -- FIXME do something with errs
  --createSnapReport theComms theEtrans fetchedQuotes
  return fetchedQuotes
  
  --let fetchableComms = filter fetchRequired theComms

createSnapReport :: [Comm] -> [Etran] -> [StockQuote] -> [String]
createSnapReport theComms theEtrans fetchedQuotes =
  lines2 ++ indexLines
  where
    sortedEtrans = sortBy (comparing $ etSym) theEtrans
    grpEtrans = groupBy (\x y -> (etSym x) == (etSym y)) sortedEtrans
    agg etrans =
        (sym , qty, want, price, amount, profit, chgpc, oops)
        where
          qty = qtys etrans
          sym = etSym $ head etrans
          comm = find (\c -> cmSym c == sym) theComms
          ctype = fmap cmType  comm
          ticker = fmap cmYepic comm
          msq = find (\s -> Just (sqTicker s) == ticker) fetchedQuotes
          (price, chg, chgpc, oops) = case msq of
            Just s -> (sqPrice s, sqChg s, sqChgpc s, "")
            Nothing -> (0.0, 0.0, 0.0, "* ERR")
          want = qty > 0 && (ctype == Just "YAFI")
          amount = qty * price / 100.0
          profit = qty * chg  /100.0

    aggEtrans = map agg  grpEtrans
    hitEtrans = filter sel3 aggEtrans
    etrans1 = sortBy (comparing $ sel1) hitEtrans
    tAmount = sum $ map sel5 etrans1
    tProfit = sum $ map sel6 etrans1
    tPc = tProfit/(tAmount - tProfit) * 100.0
    etrans2 = etrans1 ++ [ ("TOTAL", 0.0, True, 0.0, tAmount, tProfit, tPc, "")]
    texy (sym, qty, want, price, amount, profit, chgpc, oops) =
        s1 ++ s2 ++ s3
        where
          s1 = printf "%5s %12.2f " (sym::String) (qty::Double)
          s2 = printf "%12.2f %12.2f "  (price::Double) (amount::Double)
          s3 = printf "%12.2f %5.2f %s" (profit::Double) (chgpc::Double) (oops::String)


    lines2 = map texy etrans2

    index idx = case (find (\q -> idx == sqTicker q) fetchedQuotes) of
        Just sq -> texy (idx, 0.0, True, 0.0, (sqPrice sq), (sqChg sq), (sqChgpc sq), "")
        Nothing -> idx ++ " not found"

    indices = map cmYepic $ filter (\c -> "INDX" == cmType c) theComms
    
    indexLines = map index indices
