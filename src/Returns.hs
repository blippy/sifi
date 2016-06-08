module Returns where

import Text.Printf

import Comm
import Etran
import ListUtils
import Types
import Utils


data Return = Return { rtIdx::Int
                     , rtDstamp::Dstamp
                     , rtMine::Double
                     , rtMinepc::Percent
                     , rtAsx::Double
                     , rtAsxpc::Percent
                     , rtOutpc::Percent
                     } deriving (Show)


----------------------------------------------------------------------
-- parsing

mkReturn :: [String] -> Return
mkReturn ["return", arg2, dstamp, arg4, arg5] =
  Return { rtIdx = idx , rtDstamp = dstamp
         , rtMine = (asDouble arg4), rtMinepc = 0
         , rtAsx = (asDouble arg5), rtAsxpc = 0, rtOutpc = 0 }
  where idx = (read arg2)::Int

-- FIXME re-usable
mkReturns ledger = map mkReturn $ filter (\r -> "return" == (head r)) $ ldInputs ledger

----------------------------------------------------------------------

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




createReturns :: Ledger -> [String]
--createReturns' _  [] = [] -- in case no returns have been specified
createReturns ledger =
  if length rets < 2  then [] else outLine
  where
    hdr = "IDX     DSTAMP   MINE  MINE%  ASX   ASX%   OUT%"
    rets = mkReturns ledger --returns ledger
    ret0 = head rets
    lastRet = last rets
    finIdx = 1 + (rtIdx lastRet)
    
    rat = let nonUt = filter (\e -> "ut" /= etFolio e) (etrans ledger) --FIXME generalise
              cp func = unPennies $ countPennies $ map func nonUt -- FIXME genetralise
              --mine_g = unPennies $ countPennies $ map etPdp nonUt -- FIXME generalise
              --mine_bd = unPennies $ countPennies $ map etVbd nonUt    
          in (cp etPdp) / (cp etVbd) + 1.0

    finMine = (rtMine lastRet) * rat
    asxNow = commEndPriceOrDie (comms ledger) "^FTAS" -- FIXME generalise
    finRet = Return finIdx (end ledger) finMine 0 asxNow 0 0

    augReturns = rets ++ [finRet] 
    cr1 = accum deltaReturns  ret0 augReturns
    cr2 = map fmtReturn cr1
 
    summary = summaryLine $ deltaReturns finRet ret0
    outLine = [hdr] ++ cr2 ++ [summary]
