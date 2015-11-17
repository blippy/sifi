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
createCgtReport:: Ledger -> [String]
createCgtReport ledger =
  map mkRow es2
  where
    esTax = filter etTaxable $  etrans ledger

    -- find the comms which have sales during the period
    commSym = cmSym . etComm -- the symbol of a commodity
    cs = nub $ sort $ map commSym  $ filter (\e -> (etDuring e) && (etIsSell e)) esTax

    -- find those etrans which have comms that have sales
    es1 = filter (\e -> elem (commSym e) cs) esTax

    es2 = sortWith (\e -> (etSym e, etDstamp e)) es1



