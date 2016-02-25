module Nacc where

import Data.List
--import Data.Tuple.Select
import Text.Printf

import Types
import Utils


alt :: Acc -> [Nacc] -> Acc
alt acc naccs =
  altAcc
  where
    nacc = find (\n -> acc == (ncAcc n)) naccs
    altAcc = case nacc of
      Just n -> ncAlt n
      Nothing -> "Error: couldn't find alt nacc" -- FIXME do better than this


showAcc :: Acc -> String
showAcc acc = printf "%-6.6s" acc


showNaccAcc :: Nacc -> String
showNaccAcc nacc = showAcc $ ncAcc nacc

showNacc :: Nacc -> String
showNacc nacc =
  let Nacc _ acc _ _ desc = nacc in
  (showAcc acc) ++ " " ++ desc
  --printf "%-6.6s  %s" acc desc
