(*Ecrire la fonction qui, ´etant donn´ee 2 valeurs, d´etermine l’ancˆetre commun le ´
plus petit dans un arbre de recherche binaire. On suppose que les 2 valeurs apparaissent dans
l’arbre.
*)

(* Défintion d'arbre binaire*)

type t = Node of t * int * t | Nil


(*Ecrire la fonction qui trouve le trajet jusq'à la valeur, le résultat est sous la forme d'une liste *)
let rec path_to x  t =
  match t with
    | Nil -> []
    | Node(_, y , _) when y = x -> [] 
    | Node(l,y, r) -> if y >= x then y :: path_to x l else y :: path_to x r


(* Ecrire la fonction qui retourne la liste des préfixes communs entre deux listes
Dans notre cas un préfix commun entre deux listes (qui sont les trajets réspectives aux valeurs recherchées
sont les ancètres communs de ce dernièrs)*)
let rec common_prefix l1 l2 =
  match l1 , l2 with
  | x1 :: t1 , x2 :: t2  when x1 = x2 -> x1 :: common_prefix t1 t2
  | _ -> []

let () = assert (common_prefix [17; 12; 11] [17 ; 12] = [17; 12])

(* Maintenant on implémente la fonction qui retourne le minimum de ces ancètres communs en utilisant les fonctions
précdedentes*)

let min_ancestor x y t =
  let ancestors_x = path_to x t in
  let ancestors_y = path_to y t in
  let common_ancestors = common_prefix ancestors_x ancestors_y in
  List.fold_left min max_int common_ancestors



(*
            7
          /   \
        3       10  
      /   \    /  \ 
    1      5  8     11
*)
let t = Node(
  Node(
    Node(Nil, 1,Nil) ,
     3 ,
      Node(Nil, 5, Nil)
      )
  , 7
  ,Node(
    Node(Nil, 8 , Nil)
    , 10 
    , Node(Nil, 11, Nil)
  )
);;

   
(* Testing paths to variables in binary search tree*)
let () = assert (path_to 5 t = [7; 3])
let () = assert (path_to 11 t = [7; 10])


(* Testing common ancestors search*)
let () = assert (common_prefix (path_to 5 t) (path_to 11 t) = [7])


(* Testing  finding minimum common ancestor*)
let () = assert (min_ancestor 5 11 t = 7 )




