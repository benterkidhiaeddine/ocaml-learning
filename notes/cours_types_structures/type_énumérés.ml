type color = Blanc | Noir | Vert | Rouge;;


let inverse_color c = 
  match c with 
  | Blanc -> Noir
  | Noir -> Blanc
  | Vert -> Rouge
  | Rouge -> Vert



(* les types énumérés sont appelés Variants in the english version *)
type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat


(* Il n y a pas moyen d'associer directement un Enuméré avec un Entier il  faut créer une fonciton*)
let day_to_int d =
  match d with 
  | Sun -> 1
  | Mon -> 2
  | Tue -> 3
  | Wed -> 4
  | Thu -> 5
  | Fri -> 6
  | Sat -> 7


(* The variant values are called constructors and they must start with an upper case letter*)


