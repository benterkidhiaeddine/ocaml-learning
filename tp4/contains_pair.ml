let rec contains_pair lst = 
  match lst with 
    | [] -> false
    | x :: t ->  if x mod 2 = 0 then true else contains_pair t


let () = assert (contains_pair  [11; 7 ;6 ;5 ;1] = true)
let () = assert (contains_pair  [11; 7 ;9 ;5 ;1] = false)
let () = assert (contains_pair [] = false)
let () = assert (contains_pair  [22] = true)
