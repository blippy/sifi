module Parser (radi, foldLine, getYahoos) where

import Control.Monad
--import Control.Monad.IO.Class
import Data.Char
import Data.Maybe
import System.Directory
import System.Path.Glob

import ConfigParser
import Types
import Utils hiding (spaces)

filterInputs inputs =
  filter (\x -> isAlpha (x !! 0)) nonblanks
  where all = (lines . unlines) inputs
        nonblanks = filter (\x -> length x > 0) all






-- FIXME I don't think the parser handles "" correctly (see fin with S "" for example)

eatWhite "" = ""
eatWhite ('#':xs) = ""
eatWhite (x:xs) = if isSpace x then eatWhite xs else x:xs

       

-- TODO doesn't parse the empty string
-- TODO fix bug where this is not a termination by a "
getQuoted ::String -> (String, String)
--getQuoted ('"':'"':xs) = ("", xs) -- an empty string
getQuoted str =
  (h, rest)
  where (h, t) = break (\x -> x == '"') (tail str)
        rest = drop 1 t


getUnquoted str = (break isSpace str)


lexeme str
  | length nonWhite == 0 = ("", "")
  | nonWhite !! 0 == '"' = (getQuoted nonWhite)
  | otherwise = (getUnquoted nonWhite)
  where nonWhite = eatWhite str

foldLine' acc str
  | length lex == 0 = acc
  | otherwise = foldLine' (acc ++ [lex]) rest
  where (lex, rest) = lexeme str
    
foldLine str = foldLine' [] str


fileList :: IO [String]
fileList = do
  globs1 <- readConf
  yglobs <- yahooGlobs
  let globs2 = globs1 ++ [yglobs]
  let files = mapM glob globs2
  g <- files
  let h = concat g
  return h


readInputs = do
  files <- fileList
  --print files
  contents <- mapM readFile files
  let allLines = filterInputs contents
  let commands = map foldLine allLines
  return commands


matchHeads str = filter (\x -> head x == str)

makeTypes maker match  inputs = map maker $ matchHeads match inputs


mkComm :: [[Char]] -> Comm
mkComm ["comm", sym, fetch, ctype, unit, yepic, name] = 
    Comm sym bfetch ctype unit yepic name Nothing Nothing
    where bfetch = (fetch == "W")


getComms inputs = makeTypes mkComm "comm" inputs


mkDps :: [[Char]] -> Dps
mkDps fields =
  Dps (map toUpper esym) dps
  where
    ["dps", esym, dpsStr ] = fields
    dps = --FIXME this should be abstracted (e.g. also in Yahoo.hs)
      case asEitherDouble dpsStr of
        Left msg -> error $ unlines ["mkDps double error conversion", show fields]
        Right v -> v

getDpss = makeTypes mkDps "dps"


mkEtran :: [[Char]] -> Etran
mkEtran fields =
    Etran dstamp True etIsBuy folio sym qtyD amountP Nothing Nothing
    where
      [_, dstamp, way, folio, sym, qty, amount] = fields
      getDouble field name = --FIXME this should be abstracted (e.g. also in Yahoo.hs)
        case asEitherDouble field of
          Left msg -> error $ unlines ["mkEtran parseError", name, show fields, msg]
          Right v -> v
      etIsBuy = way == "B"
      sgn1 = if etIsBuy then 1.0 else (-1.0) :: Double
      qtyD = (getDouble qty "qty") * sgn1
      amountP = enPennies (sgn1 * (getDouble amount "amount"))

getEtrans = makeTypes mkEtran "etran"

mkEtranx :: [String] ->Etran
mkEtranx fields =
  et { etTaxable = False }
  where
    et = mkEtran fields


{-
mkFinancial :: [String] -> Financial
mkFinancial ["fin", action', param1', param2'] =
  f
  where
    act = if (length action') > 0 then head action' else
            error ("Can't have 0 length action:" ++ action' ++ param1' ++ param2')
    f = Financial {action = act, param1 = param1'
                  , param2 = param2' }


mkFinancial ["fin", "S"] =
  Financial {action = 'S', param1 = "", param2 = ""}
  
mkFinancial ["fin", "S", param1'] =
  Financial {action = 'S', param1 = param1', param2 = ""}
-}

mkFinancial :: [String] -> Financial
mkFinancial ("fin":action':params') = Financial action' params'
{-
  f
  where
    --act = if (length action') > 0 then head action' else
    --        error ("Can't have 0 length action:" ++ action' ++ param1' ++ param2')
    f = Financial {action = action', params = params'}
-}

mkFinancial oops =
  error ("Didn't understand financial:" ++ (show oops))

getFinancials = makeTypes mkFinancial "fin"




-- | alt is the alternative account to use if the transaction is before the start date
mkNacc :: [String] -> Nacc
mkNacc ["nacc", acc, alt, desc] = Nacc acc alt desc 

getNaccs = makeTypes mkNacc "nacc"


mkNtran :: [String] -> Ntran
mkNtran ["ntran", dstamp, dr, cr, pennies, clear, desc] =
  Ntran dstamp dr cr (asPennies pennies) clear desc

getNtrans = makeTypes mkNtran "ntran"


mkPeriod :: [String] -> Period
mkPeriod ["period", start, end] =
  (start, end)
  
getPeriods inputs = makeTypes mkPeriod "period" inputs


mkPort :: [String] -> Port
mkPort ("port":target:sources) = Port target sources

getPorts = makeTypes mkPort "port"



mkYahoo :: [String] -> StockQuote
mkYahoo fields =
  StockQuote dstamp tstamp ticker rox' price' chg' chgpc'
  where
    ["yahoo", dstamp, tstamp, ticker, rox, price, chg, chgpc, "P"] = fields
    getDouble field name =
      case asEitherDouble field of
        Left msg -> error $ unlines ["mkQuote parse error:", name, show fields, msg]
        Right v -> v
    rox' = getDouble rox "rox"
    price' = getDouble price "price"
    chg' = getDouble chg "chg"
    chgpc' = getDouble chgpc "chgpc"
    

getYahoos = makeTypes mkYahoo "yahoo"




mkReturn :: [String] -> Return
mkReturn ["return", arg2, arg3, arg4, arg5] =
  Return { idx = idxInt , dstamp = arg3
         , mine = (asDouble arg4), asx = (asDouble arg5) }
  where idxInt = (read arg2)::Int

getReturns inputs = makeTypes mkReturn "return" inputs


mkXacc :: [String] -> Xacc
mkXacc ("xacc":target:sources) = Xacc target sources

getXaccs  = makeTypes mkXacc "xacc"



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
      "etran" -> recs { rcEtrans = (rcEtrans recs ++ [mkEtran fields]) }
      "etranx" -> recs { rcEtrans = (rcEtrans recs ++ [mkEtranx fields]) }
      "fin" -> recs { rcFinancials = (rcFinancials recs ++ [mkFinancial fields]) }
      "nacc" -> recs { rcNaccs = (rcNaccs recs ++ [mkNacc fields]) }
      "ntran" -> recs { rcNtrans = (rcNtrans recs ++ [mkNtran fields]) }
      "period" -> recs { rcPeriods = (rcPeriods recs ++ [mkPeriod fields]) }
      "port" -> recs {rcPorts = (rcPorts recs ++ [mkPort fields]) }
      "return" -> recs { rcReturns = (rcReturns recs ++ [mkReturn fields]) }
      "xacc" -> recs { rcXaccs = (rcXaccs recs ++ [mkXacc fields]) }
      "yahoo" -> recs { rcQuotes = (rcQuotes recs ++ [mkYahoo fields]) }
      _ -> recs




radi = do
  inputs <- readInputs
  let recs = createRecs records0 inputs
  return recs
