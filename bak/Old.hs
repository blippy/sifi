-- graveyard for old code

-- from Utils.hs

-- can be replaced with GHC.Exts.sortWith
sortOnMc :: Ord b => (a -> b) -> [a] -> [a]
sortOnMc f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
