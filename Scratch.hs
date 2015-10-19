p1 v =
  x
  where
    y = head []
    x = if v then head [12] else y

    
p2 =
    let x = 10 
        y = x + 1 
    in x + y
