{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Etb where

import Control.Exception (catch, throw)
-- import Data.Function (on)
import Data.List as L
import Data.Maybe
import Data.Ord
import Data.Set as S
import Data.Version (showVersion)
import GHC.Exts
import Paths_sifi (version)
import System.IO
import Text.Printf

--import Aggregate
import Args
import Comm
import Epics
import Etran
import Financial
import Html
import Cgt
import Ledger
import ListUtils
import Nacc
import Portfolio
import Post
import Returns
import Snap
import Types
import Utils
import Yahoo

--sifiVersion = version
printVersion :: IO ()
printVersion = print version

data Option = PrinAccs | PrinCgt | PrinDpss | PrinEpics | PrinEtb | PrinEtrans
            | PrinFin | PrinPorts | PrinReturns | PrinSnap deriving (Eq)


data Rep = Rep String Option (Ledger -> [String])

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
    --Nacc _ acc _ desc = theNacc
    acc =  ncAcc theNacc
    desc = ncDesc theNacc
    runningTotals = cumPennies $ L.map postPennies ps
    body = map2 etbLine ps runningTotals
    textLines = ["Acc: " ++ acc, desc] ++ body ++ [";"]



reportAccs grp ledger = concatMap (printEtbAcc $ naccs ledger) grp



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
assembleEtb :: [Xacc] -> [[Post]] -> Etb
assembleEtb xaccs ps =
  augs
  where
    --summate (a, n, posts) = (a, countPennies (L.map postPennies posts))
    summate ps = (postDr $ head ps, countPennies (L.map postPennies ps))
    lup = L.map summate ps
    augs = augEtb xaccs lup

createEtbReport:: Etb -> Ledger -> [String]
createEtbReport etb _ =
  eLines ++  [totalLine]
  where
    sorted = sortWith fst etb
    eLine (acc, pennies) = (psr 6 acc) ++ (show pennies)
    eLines = L.map eLine sorted
    total  = countPennies $ L.map snd sorted
    totalLine = eLine ("TOTAL", total)


data Report = Report { rpTitle :: String, rpPrint :: Bool, rpBody :: String }


mkSection:: [Option] -> Ledger -> Rep -> Report
mkSection options ledger rep =
  Report title typep text
  where
    Rep title option repMaker = rep
    lines = repMaker ledger
    typep = (option == PrinSnap) && (elem option options)
    text = (upperCase title) ++ ":\n"  ++ (unlines lines) ++ "."


mkReports :: Ledger -> [Option] -> [Report]
mkReports  ledger options = 
  L.map (mkSection options ledger) repTbl
  where
    theEtrans = etrans ledger
    posts = createPostings (ntrans ledger) theEtrans      
    grp = groupPosts posts
    theXaccs = xaccs ledger
    etb = assembleEtb theXaccs grp 

    repTbl =
      [ Rep "accs"       PrinAccs    (reportAccs grp)
      , Rep "cgt"        PrinCgt     createCgtReport
      , Rep "epics"      PrinEpics   reportEpics
      , Rep "etb"        PrinEtb     (createEtbReport etb)
      , Rep "etrans"     PrinEtrans  createEtranReport
      , Rep "financials" PrinFin     (createFinancials etb)
      , Rep "portfolios" PrinPorts   createPortfolios
      , Rep "returns"    PrinReturns createReturns
      , Rep "snap"       PrinSnap    createSnapReport
      ]
        


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




createEtbDoing  options cmdOptions = do
  ledger <- ratl cmdOptions
  let reps = mkReports ledger options
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

hsnap = createEtbDoing [PrinSnap]  (defaultOptions {_optFetch = True})

createEtb = createEtbDoing optionSet0
mainEtb =  createEtbDoing optionSet0

ceb = createEtb defaultOptions
