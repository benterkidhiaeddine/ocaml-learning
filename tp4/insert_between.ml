let rec insert_between num lst = 
  match lst with 
    | [] -> []
    | [x] -> [x]
    | x :: y ::  t -> x :: num  :: insert_between num (y :: t)  

let () = assert (insert_between 20  [11; 7 ;6 ;5 ;1] =  [11; 20;  7 ; 20 ;6; 20 ;5; 20 ;1 ])
let () = assert (insert_between 20 [] = [])
let () = assert (insert_between 10  [22] = [22])
