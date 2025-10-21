let rec num_digits_rt n =
  let rec num_digits_aux n acc =
    if (abs n ) < 10 then 
      acc 
  else num_digits_aux (n / 10) (acc + 1) in
  num_digits_aux n 1;;

print_int (num_digits_rt 5579888)
