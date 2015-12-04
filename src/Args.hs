{-# LANGUAGE TemplateHaskell #-}
module Args where
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/System-Console-GetOpt.html


import Control.Lens hiding (element) -- use of lenses probably not necessary here
import Data.Maybe
--import Safe
import System.Console.GetOpt
import System.Environment

data MainAction = Init | Normal | ShowArgs | Snap | Web | Yahoo deriving Show

data Options = Options
               { _optMainAction :: MainAction
               , _optFetch :: Bool -- download prices from internet?
               , optStart :: Maybe String
               , optEnd :: Maybe String
                 
               , optOtherArgs :: [String] -- the remaining command line arguments
               } deriving Show

defaultOptions = Options
                 { _optMainAction = Normal
                 , _optFetch = False
                 , optStart = Nothing
                 , optEnd = Nothing
                            
                 , optOtherArgs = []
                 } 

makeLenses ''Options


{-
noArg:: a -> b ->  Options ->ArgDescr (Options -> Options)
noArg field newVal opts = NoArg (set field newVal)
-}

setMainAction act = NoArg (set optMainAction act)
--setMainAction act opts = NoArg (\opts -> opts {optMainAction = act})

options  :: [OptDescr (Options -> Options)]
options =
  [ Option ['a'] ["args"]  (setMainAction ShowArgs) "show arguments passed in"
  --, Option ['b'] ["browse"] (NoArg Browse) "open graphical browser"
  , Option [] ["end"] (ReqArg (\s opts -> opts { optEnd = Just s}) "END") "set the end of the period in form YYYY-MM-DD"
  , Option ['e'] ["epics"] (setMainAction Yahoo) "show share price movements for supplied epics"
  , Option [] ["init"] (setMainAction Init) "Soft initialise: create default dirs/files, no overwriting"
  , Option ['n'] ["normal"] (setMainAction Normal) "Normal operation"
  , Option ['s'] ["snap"]  (setMainAction Snap)  "show a daily snapshot"
  , Option [] ["start"] (ReqArg (\s opts -> opts { optStart = Just s}) "START") "set the start of period in form YYYY-MM-DD"
  , Option ['w'] ["web"]   (NoArg (set optFetch True))   "download, and create accounts"

  ]



compilerOpts' :: [String] -> IO (Options, [String])
compilerOpts' argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: sifi [OPTION...] files..."

compilerOpts :: [String] -> IO (Options)
compilerOpts argv = do
  (opts, rem) <- compilerOpts' argv
  --let (opts, remainder) = opts_rem
  return $ opts { optOtherArgs = rem }
  --return opts
                     

processCmdArgs = do
  argv <- getArgs
  compilerOpts argv  

