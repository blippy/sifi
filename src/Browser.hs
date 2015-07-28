module Browser (openBrowser) where

import System.Process

--import Types
import Utils

-- url is of form: "file:///C:/Users/mcarter/AppData/Local/MarkCarter/sifi/sifi.htm"
rawUrl = do
  url1 <- outFile "sifi.htm"
  return  ("file:///" ++ url1)


openLinuxBrowser = do
  url <- rawUrl
  --ret <- rawSystem "firefox" [url]
  ret <- spawnProcess "firefox" [url]
  return ()

openWinBrowser = do
   url1 <- rawUrl -- outFile "sifi.htm"
   let url2 = map (\c -> if c == '\\' then '/' else c) url1
   --let url3 = "file:///" ++ url2
   --print url2
   ret <- rawSystem "explorer" [url2]
   return ()


openBrowser :: IO ()
openBrowser = if isLinux then openLinuxBrowser else openWinBrowser

