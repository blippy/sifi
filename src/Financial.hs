module Financial where

import Data.List.Split
import Data.Maybe

import Types
import Utils



type PennyStack = [Pennies]

accStack :: Pennies -> PennyStack -> PennyStack
accStack p (x:xs) = (p |+| x):xs
accStack p [] = [Pennies 0] --error $ "accStack error: " ++ (show oops)

procPM sgn stack p1 p2 =
  (Just str, accStack (Pennies 666) stack)
  where
    val = p1
    str = "P:" ++ val ++ p2

reduceStack :: PennyStack -> PennyStack
reduceStack (x1:x2:xs) = (x1 |+| x2):xs

stackTop:: Int -> PennyStack -> (Maybe String, PennyStack)
stackTop sgn (x:xs) = (Just "TODO T/U", x:xs)

procFin :: Financial -> PennyStack  -> (Maybe String, PennyStack)
procFin fin stack =
  let (c, p1, p2) = (action fin, param1 fin, param2 fin) in
  case c of
    'I' -> (Nothing, (Pennies 0):stack)
    'M' -> procPM (-1) stack p1 p2
    'P' -> procPM 1 stack p1 p2
    'R' -> (Nothing, reduceStack stack)
    'S' -> (Just ("S:" ++ p1), stack)
    'T' -> stackTop (-1) stack
    'U' -> stackTop  1 stack
    'Z' -> (Nothing, [])
    _   -> error $ "Can't identify financial type: " ++ [c]





finMP arg1 sgn arg2 etb =
  (take 26 arg2) ++ (show  p2) ++ (drop 38 arg2)
  where
    p1 = getp etb arg1
    p2 = scalep p1 sgn

sumAccs etb acc lst =
  etb ++ [(acc, total)]
  where
    plist = map (getp etb) lst
    total = countPennies plist
    


-- | Will usually require augmented etb, as provided by augEtb
createFinancial etb fin =
  let (c, p1, p2) = (action fin, param1 fin, param2 fin) in
  case c of
    'M' -> finMP p1  (-1.0) p2 etb
    'P' -> finMP p1    1.0  p2 etb
    'S' -> p1
    _   -> error $ "Can't identify financial type: " ++ [c]

createFinancials etb userData = map (createFinancial etb) userData
