{-# LANGUAGE ScopedTypeVariables #-}

module Etb where

import Control.Exception (catch, throw)
--import Control.Monad.IfElse
--import Data.Either
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Ord
--import Data.Set (fromList)
--import Data.String.Utils
import GHC.Exts
import System.IO
import Text.Printf

import Aggregate
import Comm
--import Config
import Dps
import Epics
import Etran
import Financial
import Html
import Cgt
import Ledger
import Nacc
--import Ntran
import Portfolio
import Post
import Returns
import Snap
import Types
import Utils
import Yahoo


data Option = PrinAccs | PrinCgt | PrinDpss | PrinEpics | PrinEtb | PrinEtrans
            | PrinFin | PrinPorts | PrinReturns | PrinSnap deriving (Eq)


augEtb :: [Xacc] -> Etb ->Etb
augEtb [] etb = etb
augEtb (x:xs) etb = augEtb xs $ sumAccs etb (xcTarget x) (xcSources x)

etbLine :: Post -> Pennies -> String
etbLine post runningTotal = (showPost post) ++ (show runningTotal)
 
printEtbAcc (dr, nacc, posts) = 
  textLines
  where
    n = case nacc of -- FIXME LOW Can use an OrDie function
      Just x -> x
      Nothing -> error ("Couldn't locate account:" ++ dr)
    runningTotals = cumPennies $ map postPennies posts
    Nacc acc  _ desc = n
    accHdr = "Acc: " ++ acc
    body = map2 etbLine posts runningTotals    
    textLines = [accHdr, desc] ++ body ++ [";"]

--reportAccs :: Foldable t => t ([Char], Maybe Nacc, [Post]) -> [[Char]]
reportAccs grp =
  ["ACCS:"] ++ accs ++ ["."]
  where
    accs = concatMap printEtbAcc grp
  
assemblePosts :: [Nacc] -> [Post] -> [(Acc, Maybe Nacc, [Post])]
assemblePosts naccs posts =
  zip3 keys keyedNaccs keyPosts
  where
    sPosts = (sortOnMc postDstamp posts)
    keys = uniq $ map postDr sPosts
    keyedNaccs = map (\k -> find (\n -> k == (ncAcc n)) naccs) keys
    keyPosts = map (\k -> filter (\p -> k == (postDr  p)) sPosts) keys
    

assembleEtb :: [Xacc] -> [(Acc, Maybe Nacc, [Post])] -> [(Acc,  Pennies)]
assembleEtb xaccs es =
  augs
  where
    summate (a, n, posts) = (a, countPennies (map postPennies posts))
    lup = map summate es
    augs = augEtb xaccs lup

createEtbReport etb =
  eLines ++  [totalLine]
  where
    sorted = sortOnMc fst etb
    eLine (acc, pennies) = (psr 6 acc) ++ (show pennies)
    eLines = map eLine sorted
    total  = countPennies $ map snd sorted
    totalLine = eLine ("TOTAL", total)


data Report = Report { rpTitle :: String, rpPrint :: Bool, rpBody :: String }


mkSection:: [Option] -> (String, Option, [String]) -> Report
mkSection options (title, option, lines) =
  Report title typep text
  where
    typep = (option == PrinSnap) && (elem option options)
    text = (upperCase title) ++ ":\n"  ++ (unlines lines) ++ "."


mkReports :: Ledger -> [Option] -> IO [Report]
mkReports  ledger options = do
  let theComms = comms ledger
  let theEtrans = etrans ledger
  let posts = createPostings (ntrans ledger) theEtrans      

  let grp = assemblePosts (naccs ledger) posts -- FIXME LOW put into order
  let theXaccs = xaccs ledger
  let etb = assembleEtb theXaccs grp
  let asxNow = commEndPriceOrDie theComms "FTAS"
  let createdReturns = createReturns (end ledger) theEtrans asxNow (returns ledger)
  let fetchedQuotes = stWeb $ squotes ledger

  
  --let mkRep (title, option, lines) = Report title (typep option)  (unlines lines)
  let reps = map (mkSection options) [
        ("accs",       PrinAccs,    reportAccs grp) ,
        ("cgt",        PrinCgt,     createCgtReport theEtrans),
        ("dpss",       PrinDpss,    createDpssReport theComms theEtrans (dpss ledger) ), 
        ("epics",      PrinEpics,   reportEpics theComms  theEtrans) ,
        ("etb",        PrinEtb,     createEtbReport etb) ,
        ("etrans",     PrinEtrans,  createEtranReport theEtrans),
        ("financials", PrinFin,     createFinancials etb (financials ledger)),
        ("portfolios", PrinPorts,   createPortfolios theEtrans theComms),
        ("returns",    PrinReturns, createdReturns),
        ("snap",       PrinSnap,    createSnapReport theComms theEtrans fetchedQuotes)]
             
  return reps

noWrite :: IOError -> String -> IO ()
noWrite e str = do
  putStrLn str
  putStrLn "Try: sifi --init"
  throw e --re-raise the exception

createSingleReport dtStamp reps = do
  --let outStr = unlines $ mapMaybe single  reps
  
  let consoleStr = unlines $ map rpBody $ filter rpPrint reps
  putStrLn dtStamp
  putStrLn consoleStr

  let fileStr = unlines $ map rpBody reps
  f <- outFile "sifi.txt"
  writeFile f fileStr `catch` \(e::IOError) -> noWrite e "createSingleReport: sifi.txt: cannot write file"

  putStrLn "+ OK Finished"

fileReport :: String -> Report -> IO ()
fileReport dtStamp rep = do
  let fileName = fileSep ++ "text" ++ fileSep ++ (rpTitle rep) ++ ".txt"
  f <- outFile fileName
  let contents = dtStamp ++ "\n\n" ++ (rpBody rep)
  writeFile f contents


fileReports :: String -> [Report] -> IO ()
fileReports _ [] = return () -- putStr ""
fileReports dtStamp (x:xs) = do
  fileReport dtStamp x
  fileReports dtStamp xs




createEtbDoing  options downloading = do
  ledger <- ratl downloading
  reps <- mkReports ledger options
  dtStamp <- nowStr
  createSingleReport dtStamp reps
  fileReports dtStamp reps
  saveHtml



--createEtbDoing options = return ()

optionSet0 = [PrinAccs,  PrinCgt, PrinDpss, PrinEpics, PrinEtb, PrinEtrans, PrinFin, PrinPorts, PrinReturns]
optionSet1 = [PrinDpss]
optionSet2 = [PrinReturns]
optionSetX = [PrinEtb]

createSection opt = createEtbDoing [opt] -- e.g. createSection PrinReturns

createCgt = createSection PrinCgt

webYes = True
webNo = False

hsnap = createEtbDoing [PrinSnap] webYes

createEtb = createEtbDoing optionSet0 webNo
mainEtb =  createEtbDoing optionSet0
