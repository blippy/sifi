module Args where
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/System-Console-GetOpt.html

import Data.Maybe
import System.Console.GetOpt
import System.Environment

--data PrimaryAction = Args | Normal | Snap

data ArgFlag = Args | Epics | Normal | Snap | Web deriving Show

options  :: [OptDescr ArgFlag]
options =
  [ Option ['a'] ["args"]  (NoArg Args)  "show arguments passed in"
  , Option ['e'] ["epics"] (NoArg Epics) "show share price movements for supplied epics"
  , Option ['s'] ["snap"]  (NoArg Snap)  "show a daily snapshot"
  , Option ['w'] ["web"]   (NoArg Web)   "download, and create accounts"
  ]

compilerOpts :: [String] -> IO ([ArgFlag], [String])
compilerOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: sifi [OPTION...] files..."



args1 = ["-a", "--snap"]
                     




--processArgs argv = do
--  (o, n, errs) <- compilerOpts argv


processCmdArgs = do
  argv <- getArgs
  compilerOpts argv  
