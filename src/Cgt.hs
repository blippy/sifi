-- Calculate CGT for 2014/5
module Cgt where

import Data.List
import Data.List.Split
import Data.Maybe
import GHC.Exts
import Text.Printf

import Comm
import Etran
import Types


mkRow e =
  intercalate "\t" [bs, dstamp, etSym e, shareStr, priceStr, "0.00", "0.00"]
  where
    bs = if etIsBuy e then "B" else "S"
    [y, m, d] = splitOn "-" $ etDstamp e
    dstamp = intercalate "/" [d, m, y]
    shares = abs $ etQty e
    shareStr = printf "%0.4f" shares
    price = abs $  (unPennies $ etAmount e) / shares
    priceStr = printf "%0.5f" price
    
-- | create the CGT spreadsheet
createCgtReport:: Ledger -> [String] -- FIXME surely must be simplifiable?
createCgtReport ledger =
  x
  where
    es1 = filter (isJust  . etComm) (etrans ledger)
    es2 = filter etTaxable es1

    -- find the comms which have sales during the period
    --cs = commSymSold es2
    cs = nub $ sort $ map (cmSym . fromJust . etComm) $ filter etBetween $ filter etIsSell es2

    -- find those etrans which have comms that have sales
    es3 = filter (\e -> elem (cmSym $ fromJust $ etComm e) cs) es2

    --es4 = sortOnMc (\e -> (etSym e, etDstamp e)) es3
    es4 = sortWith (\e -> (etSym e, etDstamp e)) es3
    eRows = map mkRow es4
    x = eRows

