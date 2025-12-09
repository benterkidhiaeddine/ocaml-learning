(*
Trouver l’incr´ement positive maximale entre deux ´el´ements cons´ecutifs dans une
liste. Renvoier None si aucune incr´ement n’est observ´ee dans la liste, soit parce que la liste est
trop courte, soit parce que toutes les modifications cons´ecutives ne sont pas positives. Exemples :
max_incr [12; 35; 77; -72] --> Some 42 (* 35 -> 77 *)
max_incr [200; 100; 15] --> None (* no increase *)
max_incr [123; 123; 123] --> None (* no increase *)
max_incr [10] --> None (* no increase *)
*)



(* This version is way too complicated*)
let rec max_incr_v1 lst =
  let rec aux acc lst = 
  match lst, acc with 
    | [] , None -> None 
    | [] ,Some m -> Some m 
    | [x], None -> None
    | [x], Some m -> Some m
    | x :: y :: [], None -> if y > x then Some (y - x)
    else None
    | x :: y :: [], Some m -> if y > x then Some (max m (y - x))
    else Some m
    | x :: y :: t , None -> if y > x then aux (Some (y - x)) (y :: t)
    else aux None (y :: t)
    | x :: y :: t , Some m -> if y > x then aux (Some (max m (y - x))) (y :: t)
    else aux (Some m) (y :: t)
  in aux None lst

let max_incr lst =
  let rec aux acc = function
    | x :: y :: t ->
        let inc = y - x in
        let acc =
          if inc > 0 then
            match acc with
            | None -> Some inc
            | Some m -> Some (max m inc)
          else acc
        in
        aux acc (y :: t)
    | _ -> acc
  in
  aux None lst
  
let () = assert (max_incr [ 12; 35; 77; -72] = Some 42 )
let () = assert (max_incr [12; 35] = Some 23)
let () = assert (max_incr [200; 100 ; 15] = None)
let () = assert (max_incr [123; 123 ; 123] = None)
let () = assert (max_incr [10] = None)
let () = assert (max_incr [] = None)