let max_of_three x y z =

  if x >= y then
    if x >= z then
      x
    else
      z
  else
      if y >= z then
        y
      else 
        z
    
let () = 
   Printf.printf "the maximum is : %d\n " (max_of_three 8 5 9);;
   Printf.printf "the maximum is : %d\n " (max_of_three 11 5 9);;
   Printf.printf "the maximum is : %d\n" (max_of_three 8 22 9);;
   Printf.printf "the maximum is : %d\n" (max_of_three 8 8 8);;