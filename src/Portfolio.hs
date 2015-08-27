module Portfolio where

import Data.List
import Text.Printf

import Comm
import Etran
import Types
import Utils

-- FIXME there is a lot of redundant code in this module

data Flow = Flow { flName :: String
                 , flFolio :: String
                 , flBefore :: Pennies
                 , flFlow :: Pennies
                 , flProfit :: Pennies
                 , flTo :: Pennies
                 , vflRet :: Percent
                 } deriving Show

etran2flow e =
  Flow { flName = etSym e
       , flFolio = etFolio e          
       , flBefore = b
       , flFlow = etFlow e
       , flProfit = p
       , flTo = etVcd e
       , vflRet = r }
  where
    b = etVbd e
    --f = etFlow e
    p = etPdp e
    -- t = etVcd e
    r = (unPennies p) / (unPennies b) * 100.00


--etrans2flows es = map etran2flow es

pfRet profitDuring valueBefore = (unPennies profitDuring) / (unPennies valueBefore) * 100.00

-- | Given several flows, reduce them to 1
reduceFlows name  fs =
  Flow  { flName = name
       , flFolio = name
       , flBefore = b
       , flFlow = colSum flFlow 
       , flProfit = p
       , flTo = colSum flTo
       , vflRet = ret }
  where
    colSum func = countPennies $ map func fs
    p = colSum flProfit
    b = colSum flBefore
    ret = pfRet p b
  

filterFlows folioName fs = filter (\fl -> folioName == flFolio fl) fs

-- | filter flows by names
ffbns folioNames fs = filter (\fl -> elem (flFolio fl) folioNames) fs

-- | filter and reduce flows
farf folioName fs = reduceFlows folioName $  filterFlows folioName fs


baseFlows etrans =
  newFlows
  where
    fs = map etran2flow etrans
    nubs = nub $ map flFolio fs
    folioFlow folioName = farf folioName fs
    newFlows = map folioFlow nubs  


createPort newName folioNames fs =
  (subFlows, agg)
  where
    subFlows = ffbns folioNames fs
    agg = reduceFlows newName subFlows

createPorts :: [Port] -> [Flow] -> [([Flow], Flow)] -> [([Flow], Flow)]
createPorts [] fs acc = acc
createPorts (p:ps) fs acc =
  createPorts ps fs' acc'
  where
    Port tgt srcs = p
    (subFlows, agg) = createPort tgt srcs fs
    fs' = agg:fs
    acc' = acc ++ [(subFlows, agg)]

showFlow :: Flow -> String
showFlow f =
  concat [n, b, fl, p, t, ret]
  where
    n = fmtName $ flFolio f
    fmt1 func = show  $ func f
    b = fmt1 flBefore
    fl = fmt1 flFlow 
    p = fmt1 flProfit
    t = fmt1 flTo
    ret = fmtRet $ vflRet f
    
  
showPort :: String -> ([Flow], Flow) -> String 
showPort  acc ([], fEnd) =
  unlines [acc, pfSpacer1, showFlow fEnd, pfSpacer2, ""]


showPort acc (f:fs, fEnd) =
  showPort acc' (fs, fEnd)
  where
    acc' = acc ++ (showFlow f) ++ "\n"
  

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
    ret = pfRet vprofit vbefore
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
    
createIndices comms =
  --map (createIndexLine comms) ["FTAS", "FTSE", "FTMC"]
  [createIndexLine comms (cmSym c)  | c <- comms, cmType c == "INDX"]


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




createPortfolios :: [Etran] -> [Comm] -> [Port] -> [String]
createPortfolios etrans comms ports =
  --old ++ new ++ ports2 -- [show ports1]
  [pfHdr] ++ ports2 ++ (createIndices comms)
  where
    --old = (createEquityPortfolios etrans) ++ (createIndices comms)
    --new = map show $ baseFlows etrans
    ports1 = createPorts ports (baseFlows etrans) []
    ports2 = map (showPort "") ports1
    --(comps, new1) = createPort "mine" myPorts $ baseFlows etrans

    
