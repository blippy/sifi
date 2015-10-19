module Types where

import Data.Graph.Inductive.Query.Monad (mapFst)
import Data.List
import Text.Printf

type Acc = String
type Desc = String -- description
type Dstamp = String
type Etb = [(String, Pennies)]
type Folio = String
type Percent = Double -- [0, 1]
type Period = (Dstamp, Dstamp)
type Qty = Double
type Rox = Double
type Sym = String
type Ticker = String
type Tstamp = String
newtype Pennies = Pennies Integer

-- use doubles instead of floats, as you'd have problems with e.g.: enPennies (-386440.77)
enPennies :: Double -> Pennies
enPennies pounds =
  Pennies i
  where
    --d = 100.0 * float2Double pounds -- use Double for awkward rounding
    d = 100.0 * pounds
    i = (round d :: Integer)

--penTest = enPennies $ asDouble "82301.87"

unPennies :: Pennies -> Double
unPennies (Pennies p) = (fromIntegral p) / 100.0

formatDecimal d
    | d < 0.0   = "-" ++ (formatPositiveDecimal (-d))
    | otherwise = formatPositiveDecimal d
    where formatPositiveDecimal = uncurry (++) . mapFst addCommas . span (/= '.') . printf "%0.2f"
          addCommas = reverse . concat . intersperse "," . unfoldr splitIntoBlocksOfThree . reverse
          splitIntoBlocksOfThree l = case splitAt 3 l of ([], _) -> Nothing; p-> Just p


instance Show Pennies where
  --show (Pennies p) = printf "%12.2f" (unPennies (Pennies p))
  show (Pennies p) = printf "%12s" $ formatDecimal (unPennies (Pennies p))





infixl 6 |+|
Pennies a |+| Pennies b = Pennies (a+b)

infixl 6 |-|
Pennies a |-| Pennies b = Pennies (a-b)


scalep :: Pennies -> Double -> Pennies
scalep p by = enPennies( by * (unPennies p))

negPennies :: Pennies -> Pennies -- unary negate pennies
negPennies p = (Pennies 0) |-| p


negp = negPennies
    
cumPennies :: [Pennies] -> [Pennies]
--cumPennies (p:[]) = p
--cumPennies (p:ps) = p: |+| (cumPennies ps)
cumPennies ps =
  fst resultTuple
  where
    f (pennies, tot)  p =
      (pennies ++ [newTot],  newTot)
      where
        newTot = tot |+| p
    resultTuple = foldl f ([], Pennies 0) ps
      
testCumPennies = cumPennies [Pennies 3, Pennies 4, Pennies 5]
  
countPennies :: [Pennies] -> Pennies
countPennies ([]) = (Pennies 0)
countPennies (p:ps) = p |+| (countPennies ps)

testCountPennies = countPennies [(Pennies 3), (Pennies 4)]



noPennies :: Pennies -> Bool
noPennies p = 0.0 == unPennies p






data Comm = Comm { cmSym :: Sym
                 , cmFetch :: Bool
                 , cmType :: String
                 , cmUnit :: String -- currency as string, e.g. USD P GBP NIL
                 --, cmExch :: String
                 --, cmGepic :: String
                 , cmYepic :: Ticker
                 , cmName :: String
                 , cmStartPrice :: Maybe Double
                 , cmEndPrice :: Maybe Double }
            deriving Show


data Dps = Dps { dpSym::Sym
               , dpDps::Double -- dividend per share in PENCE
               } deriving (Show)

data Etran = Etran { etDstamp::Dstamp
                   , etTaxable::Bool
                   , etIsBuy::Bool
                   , etFolio::Folio
                   , etSym::Sym
                   , etQty::Qty
                   , etAmount::Pennies
                   , etDuring :: Maybe Bool
                   , etComm :: Maybe Comm }
             deriving Show

data Financial =
  Financial { action::String
            , params::[String]
            --, param1::String
            --, param2::String
            } deriving (Show)


data Nacc = Nacc { ncAcc::Acc, ncAlt::Acc, ncDesc::String} deriving Show

data Ntran = Ntran { ntDstamp::Dstamp, ntDr ::Acc, ntCr:: Acc, ntP:: Pennies
                   , ntClear:: String, ntDesc:: String} deriving Show


data Port = Port { prTarget :: Folio, prSources :: [Folio] } deriving Show            


data Return = Return { rtIdx::Int
                     , rtDstamp::Dstamp
                     , rtMine::Double
                     , rtMinepc::Percent
                     , rtAsx::Double
                     , rtAsxpc::Percent
                     , rtOutpc::Percent
                     } deriving (Show)


data StockQuote = StockQuote {
  sqDstamp :: String
  , sqTstamp :: String
  , sqTicker :: String
  , sqRox :: Double
  , sqPrice :: Double
  , sqChg :: Double
  , sqChgpc :: Double
  } deriving (Show)



data Xacc = Xacc { xcTarget :: Acc, xcSources :: [Acc] } deriving Show

-- see Parser.hs for reading these items
data Record = RecComm Comm
            | RecDps Dps
            | RecEtran Etran
            | RecFinancial Financial
            | RecNacc Nacc
            | RecNtran Ntran
            | RecPeriod Period
            | RecPort Port
            | RecQuote StockQuote
            | RecReturn Return
            | RecXacc Xacc
            deriving (Show)

data Records = Records {
  rcComms :: [Comm]
  , rcDpss :: [Dps]
  , rcEtrans :: [Etran]
  , rcFinancials :: [Financial]
  , rcNtrans :: [Ntran]
  , rcNaccs :: [Nacc]
  , rcPeriods :: [Period]
  , rcPorts :: [Port]
  , rcQuotes :: [StockQuote]
  , rcReturns :: [Return]
  , rcXaccs :: [Xacc]
  } deriving Show

records0 = Records  [] [] [] [] [] [] [] [] [] [] [] --  (take 10 $ repeat [])::Records
--records0 = Records $ ((take 11 $ repeat [])::Records)



------------------------------------------------------------------------
-- Ledger records

data StockTrip = StockTrip
                 { stFile :: [StockQuote] -- from file cache
                 , stSynth :: [StockQuote] -- synthesised stock quotes
                 , stWeb :: [StockQuote] -- stock quotes downloaded from web
                 } deriving Show

allSt:: StockTrip -> [StockQuote]
allSt (StockTrip f s w) = f ++ s ++ w
                 
data Ledger = Ledger
    {
      ldRecords :: Records
    , start :: Dstamp
    , end :: Dstamp
    , squotes :: StockTrip
    } deriving Show

comms = rcComms . ldRecords
dpss  = rcDpss . ldRecords
etrans = rcEtrans . ldRecords
financials = rcFinancials . ldRecords
ntrans = rcNtrans . ldRecords
naccs = rcNaccs . ldRecords
ports = rcPorts . ldRecords
returns = rcReturns . ldRecords
xaccs = rcXaccs . ldRecords

ledgerQuotes ledger = allSt $ squotes ledger


------------------------------------------------------------------------
