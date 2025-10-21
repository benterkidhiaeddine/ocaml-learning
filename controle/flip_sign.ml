let rec flip lst = 
  match lst with
  | [] -> []
  | h :: t -> -h :: flip t
  