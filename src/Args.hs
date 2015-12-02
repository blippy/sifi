{-# LANGUAGE TemplateHaskell #-}
module Args where
-- http://hackage.haskell.org/package/base-4.8.0.0/docs/System-Console-GetOpt.html


import Control.Lens hiding (element) -- use of lenses probably not necessary here
import Data.Maybe
--import Safe
import System.Console.GetOpt
import System.Environment

{-
data ArgFlag = Args | Browse | Epics
             | Init | Normal | Snap | Start String | Web deriving (Eq, Show)

options  :: [OptDescr ArgFlag]
options =
  [ Option ['a'] ["args"]  (NoArg Args)  "show arguments passed in"
  , Option ['b'] ["browse"] (NoArg Browse) "open graphical browser"
  , Option ['e'] ["epics"] (NoArg Epics) "show share price movements for supplied epics"
  , Option [] ["init"] (NoArg Init) "Soft initialise: create default dirs/files, no overwriting"
  , Option ['n'] ["normal"] (NoArg Normal) "Normal operation"
  , Option ['s'] ["snap"]  (NoArg Snap)  "show a daily snapshot"
  , Option [] ["start"] (ReqArg Start "START") "set the start of period in form YYY-MM-DD"
  , Option ['w'] ["web"]   (NoArg Web)   "download, and create accounts"
  ]

compilerOpts :: [String] -> IO ([ArgFlag], [String])
compilerOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: sifi [OPTION...] files..."
-}

data MainAction = Init | Normal | ShowArgs | Snap | Web | Yahoo deriving Show

data Options = Options
               { _optMainAction :: MainAction
               , optStart :: Maybe String
               , optEnd :: Maybe String
               , optOtherArgs :: [String] -- the remaining command line arguments
               } deriving Show

defaultOptions = Options
                 { _optMainAction = Normal
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
  , Option ['e'] ["epics"] (setMainAction Yahoo) "show share price movements for supplied epics"
    
  , Option [] ["init"] (setMainAction Init) "Soft initialise: create default dirs/files, no overwriting"
  , Option ['n'] ["normal"] (setMainAction Normal) "Normal operation"
  , Option ['s'] ["snap"]  (setMainAction Snap)  "show a daily snapshot"
  , Option [] ["start"] (ReqArg (\s opts -> opts { optStart = Just s}) "START") "set the start of period in form YYY-MM-DD"
  , Option ['w'] ["web"]   (setMainAction Web)   "download, and create accounts"

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

