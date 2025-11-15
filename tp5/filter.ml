let is_greater_than_2 x = x > 2 


let rec filter f lst = 
  match lst with
    | [] -> []
    | x :: t ->  if f x then x :: filter f t 
    else filter f t


let () = assert ( filter is_greater_than_2 [1;2;4;5;6] = [4;5;6])