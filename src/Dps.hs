-- Dps - dividend per share
-- these are ESTIMATED from inputs, not actual

module Dps where

import Data.Char
import Data.List
--import Data.Maybe

import Comm
import Epics
import Etran
import Portfolio
import Types
import Utils






data DpsCalc = DpsCalc {
  dcSym::Sym
  , dcDps::Double
  , dcQty::Qty
  , dcDiv::Pennies
  , dcValue::Pennies
  , dcPercent::Percent
  , dcWarn::Bool -- warn that it is assumed that DPS is 0
  } deriving (Show)

-- | Create Dps Report Line for an Epic
calcDps theDpss epic =
  DpsCalc esym dps qty div val pc w
  where
    esym = sym epic
    theDps = find (\d -> esym == dpSym d) theDpss
    (dps, w) = case theDps of
      Just d -> (dpDps d, False)
      Nothing -> (0.0, True)
    qty = eqty epic
    div = enPennies $ dps * qty / 100.0
    val = value epic
    pc = unPennies div / unPennies val
    
showDpsCalc dc =
  intercalate " " [s , dps, qty, div, val, pc, w]
  where
    s = showSym $ dcSym dc
    dps = show $ enPennies $ dcDps dc
    div = show $ dcDiv dc
    qty = showQty $ dcQty dc
    val = show $ dcValue dc
    pc = showPercent $ dcPercent dc
    w = if dcWarn dc then "*" else " "
  
createDpssReport :: [Comm] -> [Etran] -> [Dps] -> [String]
createDpssReport comms allEtrans theDpss =
  [hdr] ++ dLines ++ [tLine, warn]
  where
    --    " APH         1.09    16851.000       183.68      7414.44    2.48 *"
    hdr = " SYM          DPS          QTY          DIV        VALUE     YLD W"
    (epics, _) = reduceEtrans comms $ myFolio allEtrans
    calcs = map (calcDps theDpss) epics
    dLines = map showDpsCalc calcs
    tDiv = countPennies $ map dcDiv calcs
    tValue = countPennies $ map dcValue calcs
    tYield = unPennies tDiv/unPennies tValue
    tLine = intercalate " " ["SUM:", spacePennies, spaceQty, show tDiv, show tValue, showPercent tYield]
    warn = "* means user did not provide DPS"
