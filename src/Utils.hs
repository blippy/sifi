{-# LANGUAGE TemplateHaskell, DoAndIfThenElse, NoOverloadedStrings, TypeSynonymInstances, GADTs, CPP #-}

module Utils  where

import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Time.LocalTime
--import GHC.Exts
import GHC.Float
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
--import Safe as S
import System.Directory
import System.Environment (getEnv)
import System.Info (os)
import System.Locale (defaultTimeLocale)
import Text.Printf
import Text.Read (readMaybe)


import Types

spaces n = replicate n ' '
spacePennies = spaces 12

asPennies :: String -> Pennies -- String of form #0.00
--asPennies pounds = enPennies (asFloat pounds)
asPennies pounds = enPennies (asDouble pounds)



--instance Show Percent where
spacePercent = spaces 7
showPercent p = printf "%7.2f" $ p * 100.0

spaceQty = spaces 12
showQty q = printf "%12.3f" (q::Double)

spaceSym = spaces 4
showSym s = printf "%4.4s" s


  




stripChars :: String -> String -> String
stripChars = filter . flip notElem

clean = stripChars "\"%\n+"


asDouble :: String -> Double
asDouble v = read(clean v) :: Double


asEitherDouble str =
  case (readMaybe $ clean str) :: Maybe Double of
    Just num -> Right num
    Nothing -> Left $  "Bad Double: '" ++ str ++ "'"



asMaybeDouble :: String -> Maybe Double
asMaybeDouble str = readMaybe $ clean str 


    
  
printn n  lst = mapM_ print  (take n lst)
printAll lst = mapM_ print lst




 

putAll alist =  mapM_ putStrLn alist

getp etb key = fromMaybe (Pennies 0) (lookup key etb)


doOrDie maybeX oops =
  case maybeX of
    Just x -> x
    Nothing -> error oops

 
gainpc :: Double -> Double -> Double
gainpc num denom = 100.0 * num / denom - 100.0   

lookupOrDie what table oopsText =
  case (lookup what table) of
    Just v -> v
    Nothing -> error oopsText

testlod1 = lookupOrDie 30 [(30, 31), (32, 33)] "not printed"
testlod2 = lookupOrDie 34 [(30, 31), (32, 33)] "you shall not pass"


true x = True -- function which always returns true






-----------------------------------------------------------------------
-- printing routines

f3 :: Double -> String
f3 f = -- show a 3dp float as a string
  printf "%12.3f" f

f4 :: Double -> String
f4 f = -- show a 4dp float as a string
  printf "%12.4f" f
  
psr :: Int -> String -> String
psr n str = -- pad string right to length n
  let fmt = "%-" ++ (show n) ++ "." ++ (show n) ++ "s" in
  printf fmt str





-----------------------------------------------------------------------
-- date/time functions

now = getZonedTime


time1 :: IO LocalTime
time1 = fmap zonedTimeToLocalTime getZonedTime

time2 :: IO (String, String)
time2 = do
  t1 <- time1
  let t2 = show t1
  let ds = take 10 t2
  let ts = drop 11 t2
  return (ds, ts)

dateString :: IO String
dateString = do
  (ds, _) <- time2
  return ds

timeString :: IO String
timeString = do
  (_, ts) <- time2
  return (take 8 ts)


nowStr = do
  ds <- dateString
  ts <- timeString
  return $ intercalate " " ["DTSTAMP:", ds, ts]

printNow = do
  str <- nowStr
  putStrLn str

-----------------------------------------------------------------------
-- Files and directories


isLinux = os == "linux"

iops :: IO String -> String -> IO String
iops iostr str = do
  str1 <- iostr
  return (str1 ++ str)


outDir :: IO String
outDir =
  case os of
  "linux" -> iops (getEnv "HOME") "/.sifi"
  _ -> iops (getEnv "USERPROFILE") "\\AppData\\Local\\MarkCarter\\sifi"
  

fileSep = if isLinux then "/" else "\\"

outFile :: String -> IO String
outFile name = iops outDir (fileSep ++ name)

yahooGlobs:: IO String
yahooGlobs = outFile $ "yahoo" ++ fileSep ++  "*.txt"

-- |Create dir in standard location if missing
cdis :: String -> IO ()
cdis dir = outFile dir >>= createDirectoryIfMissing True

initDirs :: IO ()
initDirs = mapM_  cdis [ "db", "text", "yahoo" ]

-----------------------------------------------------------------------
-- Misc routines

ones = (1.0::Double)  : ones -- infinited list of 1.0's

upperCase = map toUpper

-- example usage: x = [], v2 = $(headQ 'x)
headQ xs = [| assert (not (null $(varE xs))) (head $(varE xs)) |]
