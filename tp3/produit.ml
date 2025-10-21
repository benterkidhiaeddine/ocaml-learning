(*Solution avec le paterrn matching*)
let rec produit l = 
  match l with
    | [] -> 1
    | 0 :: t -> 0
    | h :: t -> h * produit t;; 


let () = assert (produit [2 ; 5] = 10 )
let () = assert (produit [] = 1)
let () = assert (produit [2 ; 0 ; 5] = 0 )

(* Solution avec les fonctions du Module List *)
let rec produit_v2 l =
  if l = [] then 1
  else if List.hd l  = 0 then 0
  else List.hd l * produit_v2 (List.tl l)

let () = assert (produit_v2 [2 ; 5] = 10 )
let () = assert (produit_v2 [] = 1)
let () = assert (produit_v2 [2 ; 0 ; 5] = 0 )

