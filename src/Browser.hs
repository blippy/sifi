module Browser (openBrowser) where

import System.Process

--import Types
import Utils

-- url is of form: "file:///C:/Users/mcarter/AppData/Local/MarkCarter/sifi/sifi.htm"

openLinuxBrowser = do
  return ()

openWinBrowser = do
   url1 <- outFile "sifi.htm"
   let url2 = map (\c -> if c == '\\' then '/' else c) url1
   let url3 = "file:///" ++ url2
   --print url3
   ret <- rawSystem "explorer" [url3]
   return ()
   --putStrLn url3
  --ret <- rawSystem "explorer" ["file:///C:/Users/mcarter/AppData/Local/MarkCarter/sifi/sifi.htm"]
  --print ret
  --putStrLn "Done" 

openBrowser :: IO ()
openBrowser = if isLinux then openLinuxBrowser else openWinBrowser

