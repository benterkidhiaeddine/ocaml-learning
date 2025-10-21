let rec slice l a b =
  match l with 
  | []  -> []
  | x :: t  -> 
    if a > 0 then
       slice t (a - 1) (b - 1) 
    else if b > 0 then
       x :: slice t 0 (b - 1) 
    else []
;;



let () = assert (slice [0;1;2;3;4;5] 2 4 = [2;3])
let () = assert (slice [0;1;2;3;4;5] 0 4 = [0;1;2;3])
let () = assert (slice [0;1;2;3;4;5] 0 0 = [])
let () = assert (slice [0;1;2;3;4;5] 0 1 = [0])