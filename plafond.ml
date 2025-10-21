let min x y = if x < y then x else y


let plafond (n:int) = fun m -> min m n


(* version 2 : écriture opluys simplifié*)
let plafond_v2 (n:int) = min n;;



print_int (plafond_v2 4 5);