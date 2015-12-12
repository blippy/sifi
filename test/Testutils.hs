



map2 f list1 list2 =
  let f' (el1, el2) = f el1 el2 in
  map f' (zip list1 list2)

testMap2 = map2 (+) [10, 11] [12, 13]

map3 f list1 list2 list3 =
  let f' (el1, el2, el3) = f el1 el2 el3 in
  map f' (zip3 list1 list2 list3)

map4 f list1 list2 list3 list4 =
  let f' (el1, el2, el3, el4) = f el1 el2 el3 el4 in
  map f' (zip4 list1 list2 list3 list4) 
