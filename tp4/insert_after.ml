let rec insert_after num lst = 
  match lst with 
    | [] -> []
    | x :: t -> x :: num :: insert_after num t  

let () = assert (insert_after 20  [11; 7 ;6 ;5 ;1] =  [11; 20;  7 ; 20 ;6; 20 ;5; 20 ;1 ; 20])
let () = assert (insert_after 20 [] = [])
let () = assert (insert_after 10  [22] = [22 ; 10])
