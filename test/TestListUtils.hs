module TestListUtils where

import ListUtils

tlud01 = [ (1,2), (3, 4), (1,1), (6,7), (6, 8)]

tlut01 = partitionWith fst tlud01
--([(1,2),(1,1)],[(3,4),(6,7),(6,8)])



tlut02 = groupWith' fst tlud01
--[[(1,2),(1,1)],[(3,4)],[(6,7),(6,8)]]
