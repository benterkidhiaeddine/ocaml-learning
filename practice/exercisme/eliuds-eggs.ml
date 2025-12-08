
(* This function accepts a number and counts the number of bit digits in the binary representation of this number*)
let rec egg_count number =
  if number = 0 then 0 
  else 
    if number mod 2 = 1 
      then 1 + egg_count (number / 2)
    else egg_count (number / 2) 
    
