(* equation of a disk (the inner circle) x^2 + y^2 < r  *)
let distance x y = x *. x  +. y *. y |> sqrt

let score (x: float) (y: float): int =
  if distance x y <= 1. then 10
  else if distance x y <= 5. then 5 
  else if  distance x y <= 10. then 1 
  else  0
