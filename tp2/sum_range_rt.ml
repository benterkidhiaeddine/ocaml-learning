let rec plus_rt a b =
  if b = 0 then a
  else plus_rt (a + 1) (b - 1)
  
let rec sum_range_rt a b = 
  let rec sum_range_aux a acc =
    if a > b then acc
    else
      sum_range_aux (a + 1) (acc + a) in

    sum_range_aux a 0;;
  
print_int (sum_range_rt 1 5);
print_endline ""
