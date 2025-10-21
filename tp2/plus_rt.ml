let rec plus_rt a b = 
  let rec  plus_aux a b aux = 
    if b = 0 then aux
    else plus_aux a (b - 1) (aux + 1)
   in
  plus_aux a b a;;


print_int (plus_rt 2 4);
print_endline ""



(* version plus intelligente*)

let rec plus_rt_v2 a b =
  if b = 0 then a
  else plus_rt_v2 (a + 1) (b - 1)
  
