let rec minlist l =
   match l with
    | [] ->  raise ( Match_failure ("the list is empty",  0, 0)) 
    | [x] -> x
    | h::r -> min h (minlist r);;


let lst1 = [];;
(* minlist lst1;;*)

(* Implémenter la fonction index qui retourne L'index de la valeur dans la liste et None sinon*)
let index v lst = 
  let rec find i lst =
    match lst with 
    | h :: t -> if h = v then Some i else find (i + 1) t
    | [] -> None
  in find 0 lst


let () = assert (index 5 [1;2;5 ;6;22] = Some 2)
let () = assert (index 0 [1;2;5 ;6;22] = None)

