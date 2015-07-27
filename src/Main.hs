module Main where


import Args
import Etb (hsnap, mainEtb, webYes, webNo)
--import Snap (hsnap)
import Yahoo (yahooEpics)
import Utils (initDirs)

main = do
  (opts, n) <- processCmdArgs
  let opt1 = if null opts then Normal else head opts
  case opt1 of
    Args -> print (opts, n)
    Epics -> yahooEpics n
    Init -> initDirs
    Normal -> mainEtb webNo
    Snap ->  hsnap
    Web -> mainEtb webYes
