{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigParser where

import Data.ByteString.Char8 as B
import Data.FileEmbed
import Text.ParserCombinators.Parsec

import Utils hiding (spaces)

lup :: [(String, String)] -> String -> String -> String
lup pairs key def =
  --case lookup (\p -> key == fst p) pairs
  case lookup key pairs
  of
    Just x -> x
    Nothing -> def


cfgFileName = "sifi.cfg"

rcFile :: IO String
rcFile = iops outDir (fileSep ++ cfgFileName)
         --where
           --cfgFile = if isLinux then ".hssarc" else "sifi.cfg"
           




cfgFile :: GenParser Char st [(String, String)]
cfgFile = do
  many comment
  many whitey
  exprs <- many expr
  return exprs

expr :: GenParser Char st (String, String)
expr = do
  key <- var
  spaces
  char '='
  spaces
  value <- (word <|> quote)
  skipMany whitey
  return (key, value)

var :: GenParser Char st String
var = many1 letter

whitey :: GenParser Char st Bool
whitey = do
  oneOf " \r\n"
  option True comment
  return True

word :: GenParser Char st String
word = do
  str <- many1 letter
  return str
                               

quote :: GenParser Char st String
quote = do
  (char '"')
  body <- many (noneOf "\"")
  (char '"')
  return body

comment ::  GenParser Char st Bool
comment = do
  (char '#')
  many (noneOf "\n")
  return True
  

defCfg = $(embedFile "resources/sifi.cfg")
cfgIO :: IO String
cfgIO = do return $ B.unpack defCfg

              
--input = "foo = \"bar * stool\" \r\n# happy \nbaz=smurf\nstuff=\"\""
parseCfg :: String -> Either ParseError [(String, String)]
parseCfg input = parse cfgFile "(Error in configuration file)" input

getCfg :: IO [(String, String)]
getCfg = do 
  input <- cfgIO
  let edict =  parseCfg input
  let dict = case edict of
        Left x -> error (show x)
        Right x -> x
  return dict



readConf :: IO [String]
readConf = do
  dict <- getCfg
  let globs = [ lup dict "globs" "*" ]
  return globs

