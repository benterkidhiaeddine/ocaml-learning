let rec contains_zero lst = 
  match lst with 
    | [] -> false
    | 0 :: t  -> true 
    | x :: t ->  contains_zero(t)


let () = assert (contains_zero [1; 5 ; 0 ; 6 ;7] == true)
let () = assert (contains_zero [1; 5 ;  6 ;7; 0] == true)
let () = assert (contains_zero [1; 5 ;  6 ;7] == false)
let () = assert (contains_zero [] == false)