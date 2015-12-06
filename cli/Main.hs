module Main where

import Data.Version (showVersion)
import Paths_sifi (version)

import Ledger


import Args
--import Browser (openBrowser)
import Etb (ceb, hsnap, mainEtb)
--import Snap (hsnap)
import Yahoo (yahooEpics)
import Utils (initDirs)


main = do
  putStrLn $ "sifi " ++ (showVersion version)
  opts <- processCmdArgs
  case (_optMainAction opts) of
    Init -> initDirs
    Normal -> mainEtb opts
    ShowArgs -> print opts
    Snap -> hsnap
    Web -> mainEtb opts
    Yahoo -> yahooEpics (optOtherArgs opts)
