let rec is_sorted lst = 
  match lst with 
    | [] -> true
    | [_] -> true
    | x :: y :: t  -> x < y && is_sorted (y :: t) 


  
let () = assert (is_sorted  [1; 5 ; 9; 12 ; 20] == true)
let () = assert (is_sorted  [1; 5 ;  6 ;7; 10] == true)
let () = assert (is_sorted  [1; 5 ;  3 ;7] == false)
let () = assert (is_sorted  [] == true)
let () = assert (is_sorted  [22] == true)