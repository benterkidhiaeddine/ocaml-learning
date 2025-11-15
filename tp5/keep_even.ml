let rec filter f lst = 
  match lst with
    | [] -> []
    | x :: t ->  if f x then x :: filter f t 
    else filter f t

let is_even x = x mod 2 = 0

let keep_even lst = 
  filter is_even lst


let () = assert ( keep_even  [10; -15; 12; 13] =  [10; 12])