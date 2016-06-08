module Parser (radi, tokeFile, tokeFiles) where
--module Parser where

import Control.Monad
import Data.Char
import Data.Maybe
import Prelude as P
import Data.Sequence as S
import System.Directory
import System.Path.Glob

import ConfigParser
import Lexer
import ListUtils
import Types
import Utils hiding (spaces)



fileList :: IO [String]
fileList = do
  globs1 <- readConf
  yglobs <- yahooGlobs
  let globs2 = globs1 ++ [yglobs]
  let files = mapM glob globs2
  g <- files
  let h = concat g
  return h




-- FIXME prolly belongs in Lexer.x
tokeFile :: FilePath -> IO [[String]]
tokeFile fname = do
  contents <- readFile fname
  return $ map alexScanTokens $ lines contents

-- FIXME prolly belongs in Lexer.x
tokeFiles :: [FilePath] -> IO [[String]]
tokeFiles fnames = do
  a <- mapM tokeFile fnames
  return $ concat a
  
getDouble fields field name = --FIXME this should be abstracted (e.g. also in Yahoo.hs)
	case asEitherDouble field of
		Left msg -> error $ unlines ["parseError", name, show fields, msg]
		Right v -> v

--tokeFiles fnames = (mapM tokeFile fnames) >>= concat
  
readInputs :: IO [[String]]
readInputs = do
  files <- fileList :: IO [String]
  tokes <- tokeFiles files
  let commands = P.filter (not. P.null)  tokes
  return commands


matchHeads str = P.filter (\x -> head x == str)

makeTypes maker match  inputs = map maker $ matchHeads match inputs


mkComm :: [[Char]] -> Comm
mkComm ["comm", yepic, fetch, ctype, unit, name] = 
    Comm yepic bfetch ctype unit name undefined undefined
    where bfetch = (fetch == "W")



mkDps :: [[Char]] -> Dps
mkDps fields =
  Dps (map toUpper esym) dps
  where
    ["dps", esym, dpsStr ] = fields
    dps = --FIXME this should be abstracted (e.g. also in Yahoo.hs)
      case asEitherDouble dpsStr of
        Left msg -> error $ unlines ["mkDps double error conversion", show fields]
        Right v -> v


--mkEacc :: [String] -> Nacc
--mkEacc ["eacc", acc, alt, desc] = Nacc True acc alt desc



mkEtran :: [[Char]] -> Etran
mkEtran fields =
    Etran dstamp isTaxable etIsBuy folio sym qtyD amountP undefined undefined
    where
      ["etran", etype, dstamp, way, folio, sym, qty, amount] = fields
      gd = getDouble fields
      isTaxable = (etype == "T")
      etIsBuy = way == "B"
      sgn1 = if etIsBuy then 1.0 else (-1.0) :: Double
      qtyD = (gd qty "mkEtran:qty") * sgn1
      amountP = enPennies (sgn1 * (gd amount "mkEtran:amount"))

-- getEtrans = makeTypes mkEtran "etran"

{-
mkEtranx :: [String] ->Etran
mkEtranx fields =
  et { etTaxable = False }
  where
    et = mkEtran fields
-}



mkFinancial :: [String] -> Financial
mkFinancial ("fin":action':params') = Financial action' params'

mkFinancial oops =
  error ("Didn't understand financial:" ++ (show oops))



-- | alt is the alternative account to use if the transaction is before the start date
mkNacc :: [String] -> Nacc
mkNacc fields =
	Nacc isEquity acc alt wayv desc 
	where
		["nacc", acc, alt, ntype, way, desc] = fields
		wayv = getDouble fields way "mkNacc:way"
		isEquity = (ntype == "E")



mkNtran :: [String] -> Ntran
mkNtran ["ntran", dstamp, dr, cr, pennies, desc] =
  Ntran dstamp dr cr (asPennies pennies) desc


mkPeriod :: [String] -> Period
mkPeriod ["period", start, end] =
  (start, end)
  

mkPort :: [String] -> Port
mkPort ("port":target:sources) = Port target sources


mkYahoo :: [String] -> StockQuote
mkYahoo fields =
  StockQuote dstamp tstamp ticker rox' price' chg' chgpc'
  where
    ["yahoo", dstamp, tstamp, ticker, rox, price, chg, chgpc, "P"] = fields
{-
    getDouble field name =
      case asEitherDouble field of
        Left msg -> error $ unlines ["mkQuote parse error:", name, show fields, msg]
        Right v -> v
-}
    gd = getDouble fields
    rox' = gd rox "mkQuote:rox"
    price' = gd price "mkQuote:price"
    chg' = gd chg "mkQuote:chg"
    chgpc' = gd chgpc "mkQuote:chgpc"
    

getYahoos = makeTypes mkYahoo "yahoo"





mkXacc :: [String] -> Xacc
mkXacc ("xacc":target:sources) = Xacc target sources



-- FIXME HIGH separate out into core and non-core modules
createRecs :: Records -> [[String]] -> Records
createRecs recs [] = recs
createRecs recs ([]:xs) = recs
createRecs recs (fields:xs) =
  createRecs recs' xs
  where
    cmd:_ = fields
    recs' = case cmd of 
      "comm" -> recs { rcComms = (rcComms recs ++ [mkComm fields]) }
      "dps"  -> recs { rcDpss  = (rcDpss  recs ++ [mkDps fields]) }
      --"eacc" -> recs {rcNaccs = (rcNaccs recs ++ [mkEacc fields]) }
      "etran" -> recs { rcEtrans = (rcEtrans recs ++ [mkEtran fields]) }
      --"etranx" -> recs { rcEtrans = (rcEtrans recs ++ [mkEtranx fields]) }
      "fin" -> recs { rcFinancials = (rcFinancials recs ++ [mkFinancial fields]) }
      "nacc" -> recs { rcNaccs = (rcNaccs recs ++ [mkNacc fields]) }
      "ntran" -> recs { rcNtrans = (rcNtrans recs ++ [mkNtran fields]) }
      "period" -> recs { rcPeriods = (rcPeriods recs ++ [mkPeriod fields]) }
      "port" -> recs {rcPorts = (rcPorts recs ++ [mkPort fields]) }
      --"return" -> recs { rcReturns = (rcReturns recs ++ [mkReturn fields]) }
      "xacc" -> recs { rcXaccs = (rcXaccs recs ++ [mkXacc fields]) }
      "yahoo" -> recs { rcQuotes = (rcQuotes recs ++ [mkYahoo fields]) }
      _ -> recs


{-
recsTable = [ ("comm", rcComms, mkComm),  ("dps", rcDpss, mkDps)
            , ("eacc", rcNaccs, mkEacc),  ("etran", rcEtrans, mkEtran)
            , ("etranx", rcEtrans, mkEtran)
            , ("fin", rcFinancials, mkFinancial)
            , ("nacc", rcNaccs, mkNacc), ("ntran", rcNtrans, mkNtran)
            , ("period", rcPeriods, mkPeriod)
            , ("port", rcPorts, mkPort), ("return", rcReturns, mkReturn)
            , ("xacc", rcXaccs, mkXacc), ("yahoo", rcQuotes, mkYahoo)]
-}

--mkEmptyTable = map (\r -> fst recTable

{-           
newWay :: IO [(String, [[String]])]
newWay = do
  inputs <- readInputs
  let grps = groupWith' head inputs
  return $ map (\x -> (head $ head x, x)) grps
-}

--newWay1 :: IO [[[String]]]
--newWay1 = readInputs >>= (\x -> groupWith' head x)

radi :: IO ([[String]], Records)
radi = do
  inputs <- readInputs
  let recs = createRecs records0 inputs
  return (inputs, recs)
