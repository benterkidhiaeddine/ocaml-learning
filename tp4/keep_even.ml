let rec keep_even lst = 
  match lst with 
    | [] -> []
    | x :: t ->  if x mod 2 = 0 then x :: keep_even t else
      keep_even t


let () = assert (keep_even  [11; 7 ;6 ;5 ;1] = [6])
let () = assert (keep_even  [12; 7 ;6 ;5 ;4] = [12; 6; 4])
let () = assert (keep_even [] = [])
let () = assert (keep_even  [22] = [22])
