module Epics where

import Data.List
import GHC.Exts
import Text.Printf

--import Aggregate
import Comm
import Etran
import Portfolio
import Types
import Utils

data Epic = Epic { sym::Sym, eqty::Double , ucost::Double, uvalue::Double
                 , cost::Pennies, value::Pennies, ret::Double} deriving (Show)

showEpic :: (Int, Epic) -> String
showEpic (n, epic) =
  printf fmt n s q uc uv c v r
  where
    fmt = "%3d %-5s %8.2f %8.2f %8.2f %s %s %6.2f"
    s = sym epic
    q = eqty epic
    uc = ucost epic
    uv = uvalue epic
    c = show $ cost epic
    v = show $ value epic
    r = ret epic

epicHdr = "IDX SYM        QTY    UCOST   UVALUE         COST        VALUE   RET%"  
every e = True

epicSum :: Pennies -> Pennies -> String
epicSum inCost inValue =
  printf "%36s %s %s" " " (show inCost) (show inValue)



deltaEtrans (inQty, inCost) e =
  (inQty+eQty, inCost |+| incCost)
  where
    eQty = etQty e
    incCost = if etIsBuy e -- incremental cost
              then (etAmount e)
              else (scalep inCost (eQty/ inQty))

  
    
processSymGrp comms etrans =
  Epic { sym = theSym, eqty = theQty, ucost = theUcost, uvalue = theUvalue
       , cost = theCost, value = theValue, ret = theRet}

  where
    theSym = etSym $ head etrans
    sortedEtrans = sortOnMc etDstamp etrans
    --(theQty, theCost) =  foldEtrans  0.0 (Pennies 0) sortedEtrans
    (theQty, theCost) = foldl deltaEtrans (0.0, Pennies 0) sortedEtrans
    theUcost = 100.0 * (unPennies theCost) / theQty
    theUvalue = commEndPriceOrDie comms theSym
    theValue = enPennies (0.01 * theQty * theUvalue)
    theRet = gainpc (unPennies theValue)  (unPennies theCost)

        
reduceEtrans comms etrans =
  (nonzeros, zeros)
  where
    --symGrp = groupByKey etSym etrans
    symGrp = groupWith etSym etrans
    epics = map (processSymGrp comms) symGrp
    (nonzeros, zeros) = partition (\e -> (eqty e) > 0.0) epics
                        
reportOn title comms etrans =
  (fullTableLines, zeroLines)
  where    
    (nonzeros, zeros) = reduceEtrans comms etrans
    tableLines = map showEpic $ zip [1..] nonzeros
    
    tCost = countPennies $ map cost nonzeros
    tValue = countPennies $ map value nonzeros
    sumLine = epicSum tCost tValue

    fullTitle = "EPICS: " ++ title
    fullTableLines = [fullTitle, epicHdr] ++ tableLines ++ [sumLine]
    zeroLines = map sym zeros 

subEpicsReportWithTitle title comms etrans cmp aFolio =   
  fst $ reportOn title comms $ feopn aFolio cmp etrans

    
subEpicsReport comms etrans cmp aFolio =
  subEpicsReportWithTitle aFolio comms etrans cmp aFolio
    

reportEpics :: Ledger -> [String]
reportEpics ledger =
  nonUts ++ nzTab ++ zTab1 ++ subReports
  where
    theEtrans = etrans ledger
    etransBySym = sortOnMc etSym theEtrans --FIXME work around apparent groupBy bug
    theComms = comms ledger

    -- FIXME looks like a lot of generalisation required here
    (nzTab, zTab) = reportOn "ALL" theComms etransBySym
    zTab1 = ["EPICS: ZEROS"] ++ zTab ++ [";"]
    -- folios = ["hal", "hl", "tdi", "tdn", "ut"]
    folios = map ncAcc $ filter ncEquity $ naccs ledger
    (nonUts, _) =  reportOn "NON-UT" theComms $ myFolio theEtrans
    subReports = concatMap (subEpicsReport theComms etransBySym (==)) folios
