(*
Impl´ementer une fonction qui trouve l’´el´ement minimal dans une liste
Comme nous avons d´ej`a discut´e, cette fonction ne marche pas (elle va lancer une exception
Match_failure) si la liste est vide. De plus, le compilateur donnera un avertissement indiquant
que la correspondance de motifs n’est pas exhaustive
*)

let rec minlist lst =
  match lst with 
  | [x] -> x
  | h :: t -> min h (minlist t)
  | [] -> Match_failure "La liste est vide elle peut pas avoir un minimum" 0 0