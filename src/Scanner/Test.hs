module Scanner.Test where

import Scanner.Lexer


lexTest = do
  s <- readFile "/home/mcarter/.sifi/data.txt"
  --mapM_ print (alexScanTokens s)
  let ts = unlines $ map show $ alexScanTokens s
  writeFile "/home/mcarter/.sifi/tokens.txt" ts
  
