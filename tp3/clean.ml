let rec invert lst = 
  let rec aux acc lst =
  match lst with
  | [] -> acc
  | h :: t -> aux  (h :: acc) t    
  in 
  aux [] lst

let rec found l a =
  match l with 
   | [] -> false
   | h :: t -> h = a || found t a

let rec remove l a =
  match l with
   | [] -> []
   | h :: t -> if h = a then
    remove t a
   else h :: remove t a



let rec clean l =
  match l with
    | [] -> []
    | h :: t -> if found t h then
      clean (h :: remove t h)
    else h :: clean t
;;
clean [1;1;4;5;5;4];;
let () = assert (clean [1;1;4;5;5;4] =  [1;4;5]);;
clean[1;1;2;7;8;7;9;4;5;5;4];;
let () =  assert (clean[1;1;2;7;8;7;9;4;5;5;4] = [1;2;7;8;9;4;5])

