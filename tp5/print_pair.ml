let print_pair x is_pair = if is_pair then print_string x ; not is_pair 

let rec fold_left f init lst = 
  match lst with
    | [] -> init
    | x :: t ->  fold_left f (f x init) t 

  
let fold_and_print_pair lst = 
  fold_left print_pair true lst;;


fold_and_print_pair ["a" ;"b" ; "c" ; "d"]