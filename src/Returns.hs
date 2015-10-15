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
    fmt = "%3d %10s %6.2f %6.2f %4.0f %6.2f %6.2f"
    Return aIdx aDstamp aMine minepc aAsx asxpc out = ret

summaryLine :: Return -> String
summaryLine r =
  let fmt = "%3s %-10s %6s %6.2f %4s %6.2f %6.2f" in
  printf  fmt "" "AVG" "" (rtMinepc r) "" (rtAsxpc r) (rtOutpc r)



deltaReturns r prev  =
  r { rtMinepc = minepc , rtAsxpc = asxpc, rtOutpc = outpc }
  where
    gap = (fromIntegral $ (rtIdx r) - (rtIdx prev))::Double
    gpc func =  100.0* ((func r) / (func prev)) ** (1.0/gap) - 100.0
    minepc = gpc rtMine
    asxpc = gpc rtAsx
    outpc    = minepc - asxpc




createReturns :: Dstamp -> [Etran] -> Double -> [Return] -> [String]
createReturns _ _ _ [] = [] -- in case no returns have been specified
createReturns ds etrans asxNow returns =
  [hdr] ++ createdReturns ++ [summary]
  where
    hdr = "IDX     DSTAMP   MINE  MINE%  ASX   ASX%   OUT%"
    ret0 = head returns
    lastRet = last returns
    finIdx = 1 + (rtIdx lastRet)
    nonUt = filter (\e -> "ut" /= etFolio e) etrans
    mine_g = unPennies $ countPennies $ map etPdp nonUt
    mine_bd = unPennies $ countPennies $ map etVbd nonUt
    finMine = (rtMine lastRet) * (mine_g / mine_bd + 1.0)
    finRet = Return finIdx ds finMine 0 asxNow 0 0

    augReturns = returns ++ [finRet] 
    createdReturns1 = accum deltaReturns  ret0 augReturns
    createdReturns = map fmtReturn createdReturns1
 
    summary = summaryLine $ deltaReturns finRet ret0
