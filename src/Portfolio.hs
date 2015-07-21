module Portfolio where

import Data.List
import Text.Printf

import Comm
import Etran
import Types
import Utils

-- FIXME LOW - use calculated values of mine/b ... computed in Etb rather than working them out here

myPorts = ["hal", "hl", "tdn", "tdi"]


fmtName :: String -> String
fmtName name = printf "%5s" name

fmtRet :: Double -> String
fmtRet v = printf "%7.2f" v

getPline :: String -> [Pennies] -> String
getPline name values =
  text
  where
    [vbefore, _, vprofit, _] = values
    ret = (unPennies vprofit) / (unPennies vbefore) * 100.00
    nameStr = fmtName  name
    retStr = fmtRet ret
    text = nameStr ++ (concatMap show values) ++ retStr


createIndexLine comms sym =
  text
  where
    --c = findComm comms comm
    fmtVal v = (printf "%12.2f" v)::String
    startPrice = commStartPriceOrDie comms sym
    endPrice = commEndPriceOrDie comms sym
    profit = endPrice - startPrice
    retStr = fmtRet (profit/startPrice * 100.0)
    text = (fmtName sym) ++ (concatMap fmtVal [startPrice, 0.0, profit,  endPrice]) ++ retStr
    
createIndices comms =  map (createIndexLine comms) ["FTAS", "FTSE", "FTMC"]


pfHdr     = "FOLIO     VBEFORE       VFLOW     VPROFIT         VTO   VRET"
pfSpacer1 = "----- ----------- ----------- ----------- ----------- ------"
pfSpacer2 = "===== =========== =========== =========== =========== ======"


pfCalc title subEtrans =
  getPline title sums
  where
    --subset = filter isIn etrans
    sumField field = countPennies $ map field subEtrans
    sums = map sumField [etVbd, etFlow, etPdp, etVcd]


-- | filter etrans on portfolio name, with sorting
--feopn :: 
feopn name cmp etrans =
  sortOnMc (\e -> (etSym e, etDstamp e)) $ filter (\e -> name `cmp` etFolio e) etrans

myFolio = feopn "ut" (/=)

createEquityPortfolios etrans =
  [pfHdr, hal, hl, tdn, tdi, pfSpacer1, mine, ut, pfSpacer1, all, pfSpacer2]
  where
    pfStd folio = pfCalc folio $ filter (\e -> folio == etFolio e) etrans
    [hal, hl, tdn, tdi, ut] = map pfStd ["hal", "hl", "tdn", "tdi", "ut"]
    -- mine = pfCalc "mine" $ filter (\e -> "ut" /= etFolio e) etrans
    mine = pfCalc "mine" $ myFolio etrans
    all = pfCalc "total" etrans

           
createPortfolios etrans comms =
  (createEquityPortfolios etrans) ++ (createIndices comms)

    
