
let est_diviseur n d = n mod d = 0
let est_premier n = 
  if n < 2 then false
  else let rec aux d =
    d * d > n || not (est_diviseur n d) && aux (d + 1)
  in aux 1