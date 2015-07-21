module Epics where

import Data.List
import Text.Printf

import Aggregate
import Comm
import Etran
import Portfolio
import Types
import Utils

data Epic = Epic { sym::Sym, eqty::Double , ucost::Double, uvalue::Double
                 , cost::Pennies, value::Pennies, ret::Double} deriving (Show)

showEpic :: Epic -> String
showEpic epic =
  printf fmt s q uc uv c v r
  where
    fmt = "%5s %8.2f %8.2f %8.2f %s %s %6.2f"
    s = sym epic
    q = eqty epic
    uc = ucost epic
    uv = uvalue epic
    c = show $ cost epic
    v = show $ value epic
    r = ret epic

epicHdr = "SYM        QTY    UCOST   UVALUE         COST        VALUE   RET%"  
every e = True

epicSum :: Pennies -> Pennies -> String
epicSum inCost inValue =
  printf "%32s %s %s" " " (show inCost) (show inValue)


foldEtrans inQty inCost  ([]) = (inQty, inCost)

foldEtrans inQty inCost  (e:es) =
  foldEtrans newQty newCost es
  where
    --isBuy = ((qty e) > 0.0)
    eQty = etQty e
    newQty = inQty + eQty
    incCost = if etIsBuy e -- incremental cost
              then (etAmount e)
              else (scalep inCost (eQty/ inQty))
    newCost = inCost |+| incCost


  
    
processSymGrp comms etrans =
  Epic { sym = theSym, eqty = theQty, ucost = theUcost, uvalue = theUvalue
       , cost = theCost, value = theValue, ret = theRet}

  where
    theSym = etSym $ head etrans
    sortedEtrans = sortOnMc etDstamp etrans
    (theQty, theCost) =  foldEtrans  0.0 (Pennies 0) sortedEtrans
    theUcost = 100.0 * (unPennies theCost) / theQty
    theUvalue = commEndPriceOrDie comms theSym
    theValue = enPennies (0.01 * theQty * theUvalue)
    theRet = gainpc (unPennies theValue)  (unPennies theCost)

        
reduceEtrans comms etrans =
  (nonzeros, zeros)
  where
    symGrp = groupByKey etSym etrans
    epics = map (processSymGrp comms) symGrp
    (nonzeros, zeros) = partition (\e -> (eqty e) > 0.0) epics
                        
reportOn title comms etrans =
  (fullTableLines, zeroLines)
  where    
    (nonzeros, zeros) = reduceEtrans comms etrans
    tableLines = map showEpic nonzeros
    
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
    

reportEpics comms etrans =
  nonUts ++ nzTab ++ zTab1 ++ subReports
  where
    etransBySym = sortOnMc etSym etrans --work around apparent groupBy bug
    (nzTab, zTab) = reportOn "ALL" comms etransBySym
    zTab1 = ["EPICS: ZEROS"] ++ zTab ++ [";"]
    folios = ["hal", "hl", "tdi", "tdn", "ut"]
    (nonUts, _) =  reportOn "NON-UT" comms $ myFolio etrans
    subReports = concatMap (subEpicsReport comms etransBySym (==)) folios
