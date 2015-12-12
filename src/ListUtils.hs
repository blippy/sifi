module ListUtils where

import Data.List

-- |Accumulate a result given a function func which takes a value and
-- a state and returns a state
-- E.g.
-- 
-- >>> vs = [10, 2, 13]
--
-- >>> v1 = accum (+) 0 vs
-- [10, 12, 25]
--
-- >>> v2 = accum (\v (_, s) -> (v, v+s)) (0, 0) vs
-- [(10,10),(2,12),(13,25)]
--
-- v3 = zip vs v1
-- [(10,10),(2,12),(13,25)]
accum :: Foldable t => (a1 -> a -> a) -> a -> t a1 -> [a]
accum func init xs =
  tail $ reverse $ foldl (\acc v  -> (func v (head acc)):acc) [init] xs

map2 f list1 list2 =
  let f' (el1, el2) = f el1 el2 in
  map f' (zip list1 list2)


-- |Partition by projecting an element from a list
-- It keeps the list in its original order
partitionWith :: Eq b => (a -> b) -> [a] -> ([a], [a])
partitionWith f xs =
  if null xs then ([], []) else partition (\x -> f x == f (head xs)) xs

-- |As groupWith but does not perform an initial sorting
groupWith' :: Eq b => (t -> b) -> [t] -> [[t]]
groupWith' f xs =
  if null xs then [[]] else xs' 
  where
    (inn, out) = partitionWith f xs
    xs' = if null out then [inn] else [inn] ++ (groupWith' f out)
    
