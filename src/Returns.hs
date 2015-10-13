module Returns where

import Text.Printf

import Comm
import Etran
import Types
import Utils



fmtReturn :: Return -> String
fmtReturn ret =
  printf fmt aIdx aDstamp aMine minepc aAsx asxpc out
  where
    fmt = "%3d %11s %6.2f %6.2f %4.0f %6.2f %6.2f"
    Return aIdx aDstamp aMine minepc aAsx asxpc out = ret


  
deltaReturns r prev  =
  Return (rtIdx r) (rtDstamp r) (rtMine r) minepc (rtAsx r) asxpc out
  where
    minepc = gainpc (rtMine r) (rtMine prev)
    asxpc  = gainpc (rtAsx  r) (rtAsx  prev)
    out    = minepc - asxpc
 

summaryLine :: Double -> Double -> Double -> String
summaryLine minepa asxpa outpa =
  printf "%15s %6s %6.2f %4s %6.2f %6.2f\n" "AVG" " " minepa " " asxpa outpa

createReturns :: Dstamp -> [Etran] -> Double -> [Return] -> [String]
createReturns _ _ _ [] = [] -- in case no returns have been specified
createReturns ds etrans asxNow returns =
  [hdr] ++ createdReturns ++ [summary]
  where
    hdr = "IDX      DSTAMP   MINE  MINE%  ASX   ASX%   OUT%"
    ret0 = head returns
    lastRet = last returns
    finIdx = 1 + (rtIdx lastRet)
    nonUt = filter (\e -> "ut" /= etFolio e) etrans
    mine_g = unPennies $ countPennies $ map etPdp nonUt
    mine_bd = unPennies $ countPennies $ map etVbd nonUt
    finMine = (rtMine lastRet) * (mine_g / mine_bd + 1.0)
    finRet = Return finIdx ds finMine 0 asxNow 0 0

    augReturns = returns ++ [finRet] 
    --createdReturns = foldReturns [] ret0 augReturns
    createdReturns1 = accum deltaReturns  ret0 augReturns
    createdReturns = map fmtReturn createdReturns1
    

    -- summary line
    gpc v =
      gainpc power 1.0
      where
        fix = (fromIntegral finIdx)::Double
        inv = 1.0/ fix
        power =  v ** inv
        
    minepa = gpc (finMine/100.0)
    asx0 = rtAsx ret0
    asxpa  = gpc (asxNow/asx0)
    outpa = gpc (finMine *asx0 / asxNow / 100.0)
    summary = summaryLine minepa asxpa outpa


