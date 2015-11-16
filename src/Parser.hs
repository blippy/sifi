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


data Pstate = Pstate { psToken::Maybe String, psRest :: String } deriving Show




eatWhite "" = ""
eatWhite ('#':xs) = ""
eatWhite (x:xs) = if isSpace x then eatWhite xs else x:xs

       

getQuoted :: String -> Pstate
getQuoted str =
  Pstate (Just h) rest
  where  (h, t) = break (\x -> x == '"') (tail str)
         rest = drop 1 t             

-- testing
pstrs = ["\"hello world\" smurf"
        , "\"\" smurf"
        , "\"\""
        , "\"foo"
        ]

ptest f = map f pstrs
tgq = ptest getQuoted
tlex = ptest lexeme
tfold = ptest foldLine


getUnquoted str =
  Pstate h' t
  where
    (h, t) = break isSpace str
    h' = if null h then Nothing else Just h



lexeme str
  | length nonWhite == 0 = Pstate Nothing  ""
  | nonWhite !! 0 == '"' = (getQuoted nonWhite)
  | otherwise = (getUnquoted nonWhite)
  where nonWhite = eatWhite str



foldLine' acc str
  | null rest = acc'
  | otherwise = foldLine' acc' rest
  where
    Pstate token rest = lexeme str
    acc' = case token of
      Just token' -> acc ++ [token']
      Nothing -> acc
    
    --(lex, rest) = lexeme str

    
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
    Comm sym bfetch ctype unit yepic name undefined undefined
    where bfetch = (fetch == "W")


-- getComms inputs = makeTypes mkComm "comm" inputs


mkDps :: [[Char]] -> Dps
mkDps fields =
  Dps (map toUpper esym) dps
  where
    ["dps", esym, dpsStr ] = fields
    dps = --FIXME this should be abstracted (e.g. also in Yahoo.hs)
      case asEitherDouble dpsStr of
        Left msg -> error $ unlines ["mkDps double error conversion", show fields]
        Right v -> v

-- getDpss = makeTypes mkDps "dps"

mkEacc :: [String] -> Nacc
mkEacc ["eacc", acc, alt, desc] = Nacc True acc alt desc
--fields =
--  n { ncEquity = True }
--  where
--    n = mkNacc fields



mkEtran :: [[Char]] -> Etran
mkEtran fields =
    Etran dstamp True etIsBuy folio sym qtyD amountP undefined undefined
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

-- getEtrans = makeTypes mkEtran "etran"

mkEtranx :: [String] ->Etran
mkEtranx fields =
  et { etTaxable = False }
  where
    et = mkEtran fields




mkFinancial :: [String] -> Financial
mkFinancial ("fin":action':params') = Financial action' params'

mkFinancial oops =
  error ("Didn't understand financial:" ++ (show oops))

-- getFinancials = makeTypes mkFinancial "fin"




-- | alt is the alternative account to use if the transaction is before the start date
mkNacc :: [String] -> Nacc
mkNacc ["nacc", acc, alt, desc] = Nacc False acc alt desc 

-- getNaccs = makeTypes mkNacc "nacc"


mkNtran :: [String] -> Ntran
mkNtran ["ntran", dstamp, dr, cr, pennies, clear, desc] =
  Ntran dstamp dr cr (asPennies pennies) clear desc

-- getNtrans = makeTypes mkNtran "ntran"


mkPeriod :: [String] -> Period
mkPeriod ["period", start, end] =
  (start, end)
  
-- getPeriods inputs = makeTypes mkPeriod "period" inputs


mkPort :: [String] -> Port
mkPort ("port":target:sources) = Port target sources

-- getPorts = makeTypes mkPort "port"



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
mkReturn ["return", arg2, dstamp, arg4, arg5] =
  Return { rtIdx = idx , rtDstamp = dstamp
         , rtMine = (asDouble arg4), rtMinepc = 0
         , rtAsx = (asDouble arg5), rtAsxpc = 0, rtOutpc = 0 }
  where idx = (read arg2)::Int

-- getReturns inputs = makeTypes mkReturn "return" inputs


mkXacc :: [String] -> Xacc
mkXacc ("xacc":target:sources) = Xacc target sources

-- getXaccs  = makeTypes mkXacc "xacc"


-- FIXME HIGH use multisets and greatly simplify this
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
      "eacc" -> recs {rcNaccs = (rcNaccs recs ++ [mkEacc fields]) }
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
