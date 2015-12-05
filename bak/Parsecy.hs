-- An interesting idea, but my original method seems simpler

module Parsecy where

import Data.List as DL
import Data.Maybe
import Data.Map.Strict as DM
import Data.MultiSet as MS
import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Char as Ch
import Text.ParserCombinators.Parsec.Combinator as Co
import Text.ParserCombinators.Parsec.Prim as Pr
import Text.ParserCombinators.Parsec.Token as T

--data Rec  = Foo Int | Bar String deriving Show

import Types

{-
data PRec = Bar String
          | Foo Comm
          deriving (Show)
-}

dataParser :: Parser [Record]
dataParser = P.many pzCommand


cmdMap :: [(String, Parser Record)]
cmdMap = [("comm", pzComm)]

pzComm :: Parser Record
pzComm = do
  d1 <- pzWhiteGraphic
  d2 <- pzWhiteGraphic
  d3 <- pzWhiteGraphic
  d4 <- pzWhiteGraphic
  d5 <- pzWhiteGraphic
  d6 <- pzWhiteGraphic
  let c = Comm d1 True d3  d4 d5 d6 Nothing Nothing
  return $ RecComm c
                  
pzCommand :: Parser Record 
pzCommand = do
  pzGoo
  cmd <- pzSym -- pzGraphic
  --pzGoo
  let subcmd = DL.lookup cmd cmdMap :: Maybe (Parser Record)
  --if isNothing subcmd
  res <- case subcmd of
    Just subCommand -> subCommand
    Nothing -> unexpected $ "command " ++ cmd
  pzGoo
  let res' = if isJust subcmd then res else (unexpected $ "command " ++ cmd)
  return res'

pzWhiteGraphic :: Parser String
pzWhiteGraphic = do
  pzGoo
  pzGraphic
  
pzGraphic :: Parser String
pzGraphic = pzString <|> pzSym
--  return c

pzString :: Parser String
pzString = do
  P.char '"'
  c <- P.many $ noneOf "\"\n"
  P.char '"'
  return c

pzSym :: Parser String
pzSym = P.many1 $ P.noneOf " \r\n#"

{-
pzGoos:: Parser ()
pzGoos = P.skipMany pzGoo
-}

pzGoo :: Parser ()
pzGoo = pzWhite <|> pzComment

pzWhite :: Parser ()
pzWhite = do
  P.skipMany $ P.oneOf " \r\n"
  return ()

pzComment :: Parser ()
pzComment = do
  P.char '#'
  --Co.manyTill $ P.char '\n'
  P.many $ P.noneOf "\n"
  return ()
  
  
