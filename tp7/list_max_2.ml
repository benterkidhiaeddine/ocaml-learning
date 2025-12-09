(*
Exercice 6. Trouver la valeur maximale parmi les ´el´ements des deux listes. Exemples :
list_max2 [12; 35; 7] [18; 22] --> Some 35
list_max2 [] [22] --> Some 22
list_max2 [] [] --> None

*)


let rec list_max2 lst1 lst2 = 
  let merged = lst1 @ lst2 in 
  let rec aux acc_max lst = 
  match lst , acc_max with
    | [] , None -> None 
    | [], Some m -> Some m 
    | h :: t ,  None ->  aux (Some h) t
    | h :: t , Some m -> aux (Some (max h m)) t
  in aux None merged


let () = assert (list_max2 [12; 35 ; 7] [18; 22] = Some 35)
let () = assert (list_max2 [] [ 22] = Some 22)
let () = assert (list_max2 [] [] = None)


