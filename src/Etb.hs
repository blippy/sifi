{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Etb where

import Control.Exception (catch, throw)
--import Control.Monad.IfElse
--import Data.Either
import Data.Function (on)
import Data.List as L
import Data.Maybe
import Data.Ord
--import Data.Set (fromList)
import Data.Set as S
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



printEtbAcc:: [Nacc] -> [Post] -> [String]
printEtbAcc ns ps =
  textLines
  where
    --p1 = head ps
    p1 = $(headQ 'ps)
    dr = postDr $ p1
    msg = "printEtbAcc couldn't find nacc: '" ++ dr ++ "' in, e.g. " ++ (show p1)
    theNacc = doOrDie (find (\n -> dr == (ncAcc n)) ns) msg
    Nacc acc _ desc = theNacc
    runningTotals = cumPennies $ L.map postPennies ps
    body = map2 etbLine ps runningTotals
    textLines = ["Acc: " ++ acc, desc] ++ body ++ [";"]



reportAccs naccs grp = concatMap (printEtbAcc naccs) grp



groupPosts:: [Post] -> [[Post]]
groupPosts ps =
  groups
  where
    keys = reverse $ sort $ S.toList $ S.fromList $ L.map postDr ps
    f (done, rest) key =
      (hit2:done, rest')
      where
        (hit1, rest') = L.partition (\p -> key == (postDr  p)) rest
        hit2 = sortOn postDstamp hit1
        
    (groups, _) = L.foldl f ([], ps) keys


--assembleEtb :: [Xacc] -> [(Acc, Maybe Nacc, [Post])] -> [(Acc,  Pennies)]
assembleEtb :: [Xacc] -> [[Post]] -> [(Acc, Pennies)]
assembleEtb xaccs ps =
  augs
  where
    --summate (a, n, posts) = (a, countPennies (L.map postPennies posts))
    summate ps = (postDr $ head ps, countPennies (L.map postPennies ps))
    lup = L.map summate ps
    augs = augEtb xaccs lup

createEtbReport etb =
  eLines ++  [totalLine]
  where
    sorted = sortOnMc fst etb
    eLine (acc, pennies) = (psr 6 acc) ++ (show pennies)
    eLines = L.map eLine sorted
    total  = countPennies $ L.map snd sorted
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
  let theNaccs = naccs ledger
  let posts = createPostings (ntrans ledger) theEtrans      
  let grp = groupPosts posts
  let theXaccs = xaccs ledger
  let etb = assembleEtb theXaccs grp
  let asxNow = commEndPriceOrDie theComms "FTAS" -- FIXME generalise
  let createdReturns = createReturns (end ledger) theEtrans asxNow (returns ledger)
  let fetchedQuotes = stWeb $ squotes ledger
  let thePorts = ports ledger

  
  --let mkRep (title, option, lines) = Report title (typep option)  (unlines lines)
  let reps = L.map (mkSection options) [
        ("accs",       PrinAccs,    reportAccs theNaccs grp) ,
        ("cgt",        PrinCgt,     createCgtReport theEtrans),
        ("dpss",       PrinDpss,    createDpssReport theComms theEtrans (dpss ledger) ), 
        ("epics",      PrinEpics,   reportEpics theComms  theEtrans) ,
        ("etb",        PrinEtb,     createEtbReport etb) ,
        ("etrans",     PrinEtrans,  createEtranReport theEtrans),
        ("financials", PrinFin,     createFinancials etb (financials ledger)),
        ("portfolios", PrinPorts,   createPortfolios theEtrans theComms thePorts),
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
  
  let consoleStr = unlines $ L.map rpBody $ L.filter rpPrint reps
  putStrLn dtStamp
  putStrLn consoleStr

  let fileStr = unlines $ L.map rpBody reps
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

ceb = createEtb
