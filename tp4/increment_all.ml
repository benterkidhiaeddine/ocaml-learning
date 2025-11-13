let rec increment_all lst = 
  match lst with 
    | [] -> []
    | x :: t ->  (x + 1) :: increment_all t


let () = assert (increment_all  [11; 7 ;6 ;5 ;1] = [12; 8 ;7 ;6 ;2])

let () = assert (increment_all  [11; 7 ;9 ;5 ;1] = [12; 8 ;10 ;6 ;2])
let () = assert (increment_all [] = [])
let () = assert (increment_all  [22] = [23])
