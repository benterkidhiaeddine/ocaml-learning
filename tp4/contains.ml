let rec contains num lst = 
  match lst with 
    | [] -> false
    | x :: t  -> if x = num then  true
    else contains num t


let () = assert (contains 5 [1; 5 ; 0 ; 6 ;7] == true)
let () = assert (contains 7 [1; 5 ;  6 ;7; 0] == true)
let () = assert (contains 88 [1; 5 ;  6 ;7] == false)
let () = assert (contains  12 [] == false)