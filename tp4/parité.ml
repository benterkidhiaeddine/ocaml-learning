let rec parité lst = 
  match lst with 
    | [] -> []
    | x :: t ->  (not (x mod 2 = 0)) :: parité t


let () = assert (parité  [11; 7 ;6 ;5 ;1] = [true; true ;false ;true ;true])
let () = assert (parité [] = [])
let () = assert (parité  [22] = [false])
