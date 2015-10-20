module Aggregate where

import Data.List

import Utils

-- for uniq, use nub


--combine p (left:[]) rights = partition (p left) rights
combine' p (left:lefts) rights =
  ins:right
  where
    (ins, outs) = partition (p left) rights
    right = if (length lefts) > 0 then (combine' p lefts outs) else [outs]

-- does a join
combine p lefts rights =
  (init c, last c)
  where c = combine' p lefts rights

showFailures outs = 
  error msg
  where
    msgLines = ["Application generated combine failure:"] ++ (map show outs)
    msg = unlines msgLines
      
strictly (ins, outs) =
  if (length outs) == 0 then ins else showFailures outs

                                      
 

combineKeys leftKey rightKey lefts rights =
  combine  p lefts rights
  where  p l r = ((leftKey l) == (rightKey r))


strictlyCombineKeys  leftKey rightKey lefts rights =
  strictly $ combineKeys leftKey rightKey lefts rights
  
              
testLefts = [10, 11, 12]
testRights =  [11, 12, 11, 12, 5, 11]
testAgg1 = combine (==)  testLefts testRights
-- ([[],[11,11,11],[12,12]],[5])

testAgg2 = strictly (combine (==) testLefts testRights)


-- FIXME groupBy doesn't play well with group keys non-adjacent
groupByKey keyFunc = groupBy (\x y -> keyFunc x == keyFunc y)

groupOn  keyFunc = groupBy (\x y -> keyFunc x == keyFunc y)


nonzero f = not (0.0 == f)

finding p lst =
  foldl f (Nothing, []) lst
  where
    f (res, acc) el =
      if ( (Nothing == res) && (p el) )
      then (Just el, acc) else (res, acc ++ [el])

testFinding = finding (== 10) [12, 13, 10, 14, 10, 15]
-- => (Just 10,[12,13,14,10,15])


-- I suspect there is a weird bug remaining
{-
collate p  (l:[]) rs =
  [(Just l, hit)] ++ misses
  where
    (hit, miss) = finding (p l) rs
    misses = map (\m -> (Nothing, Just m))  miss
-}

collate _ ([]) rs = map (\m -> (Nothing, Just m))  rs

collate p (l:ls) rs =
  [(Just l, hit)] ++ (collate p ls misses)
  where
    (hit, misses) = finding (p l) rs

testCollate1 = collate (\l r -> l == r) [10] [11, 12, 10, 13]
-- => [(Just 10,Just 10),(Nothing,Just 11),(Nothing,Just 12),(Nothing,Just 13)]

testCollate2  = collate (\l r -> l == r) [10, 11] [12, 13, 11]
-- => [(Just 10,Nothing),(Just 11,Just 11),(Nothing,Just 12),(Nothing,Just 13)]

testCollate3 = collate (\l r -> l == (fst r)) [10, 11] [ (10, 20) ]

aggL1 = [(5,10), (15,11)]
aggL2 = [ (10, 20) ]
testCollate4 = collate (\l r -> (snd l) == (fst r)) aggL1 aggL2


findOn keyTarg keyList targ alist =
  find (\p -> (keyTarg targ) == (keyList p)) alist

-- | => Just (10, 20)
testFindOn1 = findOn snd fst (5, 10) aggL2

{-
uniq' acc ([])   = acc
uniq' acc (x:xs) = if (elem x acc) then (uniq' acc xs) else (uniq' (acc ++ [x]) xs)
  
uniq lst = uniq' [] lst

testUniq = uniq [10, 11, 12, 10, 3]
-}
  
