module Test.Parser where

import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Char as Ch
import Text.ParserCombinators.Parsec.Combinator as Co
import Text.ParserCombinators.Parsec.Prim as Pr
import Text.ParserCombinators.Parsec.Token as T

import Parsecy
import Types

{-
cmdMap = [("comm", pzComm)]


pzComm :: Parser Record
pzComm = do
  let c = Types.Comm "Sym" True "Type" "Unit" "Epic" "Name" Nothing Nothing
  return $ RecComm c
-}

data Spam = Hammy Int | Jammy String deriving Show
s1 = [Hammy 2, Jammy "hi"]

data Goop = Comm | Return | Gloop Bool deriving Show
g1 = Gloop True


p1 = parse dataParser "" "  foo   \n  bar # fudge   guz \nyum"

p2 = do
  text <- readFile "/home/mcarter/.sifi/data.txt"
  let res = parse dataParser "data.txt" text
  print res
  -- parse 

