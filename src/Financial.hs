module Financial where

import Text.Printf

import Types
import Utils



type PennyStack = [Pennies]

accStack :: Pennies -> PennyStack -> PennyStack
accStack p (x:xs) = (p |+| x):xs
accStack p [] = [Pennies 0]

procPM sgn stack p1 p2 =
  (Just str, accStack (Pennies 666) stack)
  where
    val = p1
    str = "P:" ++ val ++ p2


finMP key sgn title val2 val3 etb =
  printf "%-6s%-20s%s %12s %12s" key title val1 val2 val3
  where
    val1 = show $ scalep (getp etb key) sgn


sumAccs etb acc lst =
  etb ++ [(acc, total)]
  where
    plist = map (getp etb) lst
    total = countPennies plist
    

sat arr i msg =
  case (i < length arr) of
     True -> arr !! i
     False -> error msg


-- | Will usually require augmented etb, as provided by augEtb
createFinancial etb fin =
  case (act) of
    "-" -> "                           -----------  -----------  -----------"
    "=" -> "                           ===========  ===========  ==========="
    "B" -> ""
    "M" -> finMP p1  (-1.0) p2 p3 p4 etb
    "P" -> finMP p1    1.0  p2 p3 p4 etb
    "S" -> "      " ++ p1
    _   -> error $ "Can't identify financial type: " ++ act
  where
    act = action fin
    ps = params fin
    p1 = sat ps 0 $ show fin
    p2 = sat ps 1 $ show fin
    p3 = sat ps 2 $ show fin
    p4 = sat ps 3 $ show fin

createFinancials etb userData = map (createFinancial etb) userData
