let rec filter f lst = 
  match lst with
    | [] -> []
    | x :: t ->  if f x then x :: filter f t 
    else filter f t

let is_positive x = x > 0

let keep_positive lst = 
  filter is_positive lst


let () = assert ( keep_positive  [10; -15; 12; 13] =  [10; 12; 13])