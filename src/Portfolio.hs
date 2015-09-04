module Portfolio where

import Data.List
import Text.Printf

import Comm
import Etran
import Types
import Utils

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
    p = etPdp e
    r = (unPennies p) / (unPennies b) * 100.00


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
  



fmtName :: String -> String
fmtName name = printf "%5s" name

fmtRet :: Double -> String
fmtRet v = printf "%7.2f" v



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



-- | filter etrans on portfolio name, with sorting
--feopn :: 
feopn name cmp etrans =
  sortOnMc (\e -> (etSym e, etDstamp e)) $ filter (\e -> name `cmp` etFolio e) etrans

myFolio = feopn "ut" (/=) -- FIXME not sure it should be here





createPortfolios :: [Etran] -> [Comm] -> [Port] -> [String]
createPortfolios etrans comms ports =
  [pfHdr] ++ ports2 ++ (createIndices comms)
  where
    ports1 = createPorts ports (baseFlows etrans) []
    ports2 = map (showPort "") ports1

    
