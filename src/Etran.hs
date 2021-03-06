module Etran where

import Data.List
import Data.Maybe
--import Data.Tuple.Select
import GHC.Exts

import Comm
import Types
import Utils


etIsSell = not . etIsBuy

qtys :: [Etran] -> Double
qtys es = sum $ map etQty es



deriveEtran start comms e =
  e { etDuring = during, etComm = theComm }
  where
    during = start <= etDstamp e
    theComm = findComm comms (etSym e)

deriveEtrans start comms etrans = map (deriveEtran start comms) etrans

cerl :: Etran -> String
cerl etran = -- create etran report line
  text
  where
    Etran dstamp taxable isBuy acc sym qty amount _ _ = etran
    taxStr = if taxable then "T" else "-"
    unit = 100.0 * (unPennies amount) / qty
    wayStr = if isBuy then "B" else "S"
    fields = [psr 7 sym, dstamp, taxStr, wayStr, psr 3 acc
             , f3 qty, show amount, f4 unit]
    text = intercalate " " fields
             
createEtranReport :: Ledger -> [String]
createEtranReport ledger =
  [hdr] ++ eLines
  where
    --     AFUSO   2010-12-30 T B ut      1707.590     20337.40    1191.0002
    hdr = "SYM     DSTAMP     T W FOLIO        QTY       AMOUNT         UNIT"
    sortedEtrans = sortWith (\e -> (etSym e, etDstamp e)) $ etrans ledger
    eLines = map cerl sortedEtrans



-- | Profit during period
etPdp e = (etVcd e) |-| (if etDuring e then  (etAmount e) else (etVbd e))

etStartPrice e = fromJust $ cmStartPrice $ etComm e

-- | value brought down
etVbd e =
  if etDuring e then Pennies 0 else enPennies  (etStartPrice e * 0.01 * etQty e)

etEndPrice e = fromJust $ cmEndPrice $ etComm e

-- | value carried down
etVcd e = enPennies (etEndPrice e * 0.01 * etQty e)

-- | profit brought down
etPbd e =
  if etDuring e then Pennies 0 else (etVbd e) |-| (etAmount e)

-- | flow during period
etFlow e = (etVcd e) |-| (etVbd e) |-| (etPdp e)
