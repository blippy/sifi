-- Calculate CGT for 2014/5
module Cgt where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf

import Comm
--import Config
import Etran
import Portfolio
import Types
import Utils


commSymSold :: [Etran] -> [Sym]
commSymSold es =
  cs4
  where
    -- identify the comms which have sales during the period
    --es2 = filter isJust . etDerived es1
    es3 = filter etBetween $ filter etIsSell es -- sells during period
    cs1 = map (cmSym . fromJust . etComm) es3
    cs2 = Set.fromList cs1 -- to remove dupes
    cs3 = Set.toList cs2
    cs4 = sort cs3


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
createCgtReport etrans =
  x
  where
    es1 = filter (isJust  . etComm) etrans
    --es2 = feopn "tdi" (/=) es1 -- completely ignore the ISA
    es2 = filter etTaxable es1
    cs = commSymSold es2

    -- find those etrans which have comms that have sales
    es3 = filter (\e -> elem (cmSym $ fromJust $ etComm e) cs) es2

    es4 = sortOnMc (\e -> (etSym e, etDstamp e)) es3
    eRows = map mkRow es4
    x = eRows

