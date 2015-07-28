module Main where

import Data.List
import Safe

import Args
import Browser (openBrowser)
import Etb (hsnap, mainEtb, webYes, webNo)
--import Snap (hsnap)
import Yahoo (yahooEpics)
import Utils (initDirs)

noop = putStr ""

main = do
  (opts, n) <- processCmdArgs
  let opt1 = headDef Normal opts -- if null opts then Normal else head opts
  case opt1 of
    Args -> print (opts, n)
    Browse -> noop
    Epics -> yahooEpics n
    Init -> initDirs
    Normal -> mainEtb webNo
    Snap ->  hsnap
    Web -> mainEtb webYes

  if Browse `elem` opts then openBrowser else noop
