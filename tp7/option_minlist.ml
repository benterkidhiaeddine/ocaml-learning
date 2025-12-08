(*Exercice 3 Re-´ecrire l’exercice 1 un utilisant les types option :
Etant donn´e que la valeur de retour est Some ou None, nous devons la faire correspondre ´
dans l’appel r´ecursif, nous devons donc utiliser la correspondance de motifs imbriqu´es*)


let rec min_list lst =
  match lst with 
    | [] -> None
    | h :: t -> begin 
      match min_list t with
      | None -> Some h
      | Some m -> Some (min h m) 
    end


let () = assert (min_list [1 ; 6 ;8 ] = Some 1)
let () = assert (min_list [] = None)