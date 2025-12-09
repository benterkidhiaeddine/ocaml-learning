(*
Exercice 5. Trouv´e la liste des racines carr´ees. Example :
square_roots [4.0; -9.0; 5.0; -8.0] --> [Some 2.; None; Some 2.23606797749979; None]
(Vous pouvez utiliser la fonction sqrt : float -> float pour calculer les racines carr´ees
r´eelles.)
*)


let rec racines lst =
  match lst with 
    | [] -> []
    | h :: t when h < 0. -> None :: racines t
    | h :: t  -> Some (sqrt(h)) :: racines t



let () = assert (racines  [4.0; -9.0; 5.0; -8.0] = [Some 2.; None; Some 2.23606797749979; None])
let () = assert (racines  [] = [])
let () = assert (racines  [16.] = [Some 4.])