(* equation of a disk (the inner circle) x^2 + y^2 < r  *)
let score (x: float) (y: float): int =
  if x ** 2.  +. y ** 2. <= 1. then 10
  else if x ** 2. +. y ** 2. <= 25. then 5 
  else if  x ** 2.  +. y ** 2. <= 100. then 1 
  else  0
