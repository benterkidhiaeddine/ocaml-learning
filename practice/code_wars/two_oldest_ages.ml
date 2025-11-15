
let greater_than  x y = 
  if x = y then 0
  else if x > y then -1
  else 1


let two_oldest_ages ages = 
  let sorted_ages =  List.sort greater_than ages in 
  List.iter (Printf.printf "%d") sorted_ages;
  
  match sorted_ages with
    | [] -> []
    | x :: y :: t ->  y :: x :: []
    | x :: t -> [x]

;;