(*Exercice 4 Re-´ecrire l’exercice 1 un utilisant les types option et utiliser une variable accumu￾lateur,
qui m´emorise le plus petit ´el´ement jusqu’`a pr´esent :
*)


let min_list lst =
  let rec aux min_acc lst =
  match lst, min_acc with 
    (*Liste vide mais on a pas d'accumulateur : il n y a pas de minimum*)
    | [], None -> None
    (* Liste est vide mais on a un minimum ,ca survient lorsque on a terminé la récursion 
    c'est le cas de base avec une liste pas vide
    *)
    | [], Some m -> Some m 
    | h :: t,None -> aux  (Some h) t
    | h ::t, Some m -> aux  (Some (min h m)) t
  in aux None lst

let () = assert (min_list [1 ; 6 ;8 ] = Some 1)
let () = assert (min_list [] = None)