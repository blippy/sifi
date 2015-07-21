module Scratch where

import Control.Exception (catch)
import Language.Haskell.Interpreter

main = toTry `catch` handler

toTry :: IO ()
toTry = do
  s <- readFile "README.txt"
  putStrLn s

handler :: IOError -> IO ()
handler e = putStrLn "no can do"


--foo = interpret (as :: Bool) :: IO Bool
code =  "head [True,False]"
code1 = "True"
--foo = runInterpreter code
foo = do
  let resIO = eval code1 :: Interpreter String
  res <- runInterpreter $ setImports["Prelude"] >> resIO
  return res
