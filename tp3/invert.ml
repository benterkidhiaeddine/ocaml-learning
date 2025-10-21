let rec invert lst = 
  let rec aux acc lst =
  match lst with
  | [] -> acc
  | h :: t -> aux  (h :: acc) t    
  in 
  aux [] lst

