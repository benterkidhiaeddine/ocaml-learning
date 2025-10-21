(* function that just counts until n which is an optimized version*)
let rec count_aux n acc =
  if n = 0 then acc
  else count_aux (n - 1) (acc + 1)


let rec count_tr n = count_aux n 0

let add a b = a + b

let sum  = add 5 6
let () = Printf.printf "the result is %d\n" sum 

let res = (count_tr 4 )

let () = Printf.printf "the result is %d\n" res 
