let rec invert lst = 
  let rec aux acc lst =
  match lst with
  | [] -> acc
  | h :: t -> aux  (h :: acc) t    
  in 
  aux [] lst


let clean l =
  let rec aux acc tmp =
    match tmp with
    | [] -> invert acc
    | [h] -> invert (h :: acc)
    | h :: x :: t ->
        if h = x then aux acc (x :: t)
        else aux (h :: acc) (x :: t)
  in
  aux [] l
;;
let () = assert (clean [1;1;4;5;5;4] =  [1;4;5;4]);;
let () =  assert (clean[1;1;2;7;8;7;9;4;5;5;4] = [1;2;7;8;7;9;4;5;4])

