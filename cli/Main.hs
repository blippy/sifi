module Main where

import Ledger

{-
import Args
import Browser (openBrowser)
<<<<<<< HEAD:cli/Main.hs
import Etb (ceb, hsnap, mainEtb, webYes, webNo)
=======
import Etb (ceb, hsnap, mainEtb)
--import Snap (hsnap)
>>>>>>> master:src/Main.hs
import Yahoo (yahooEpics)
import Utils (initDirs)


main = do
  opts <- processCmdArgs
  case (_optMainAction opts) of
    Init -> initDirs
    Normal -> mainEtb opts
    ShowArgs -> print opts
    Snap -> hsnap
    Web -> mainEtb opts
    Yahoo -> yahooEpics (optOtherArgs opts)
-}

main = do
  print "hello"
