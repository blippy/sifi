module Main where

--import qualified Ledger as Ledger
import Ledger

{-
import Args
import Browser (openBrowser)
import Etb (ceb, hsnap, mainEtb, webYes, webNo)
import Yahoo (yahooEpics)
import Utils (initDirs)


main = do
  opts <- processCmdArgs
  case (_optMainAction opts) of
    Init -> initDirs
    Normal -> mainEtb webNo opts
    ShowArgs -> print opts
    Snap -> hsnap
    Web -> mainEtb webYes opts -- FXIME Web can be combined into Normal using an option
    Yahoo -> yahooEpics (optOtherArgs opts)
-}

main = do
  print "hello"
