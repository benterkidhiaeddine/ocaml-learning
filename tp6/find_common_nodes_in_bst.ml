(* Définir le type d'arbre binaire*)
type t = Node of t * int * t  | Nil

(* Définir l'algorithme pour faire la visite in order de l'arbre binaire *)
let rec in_order_visit t =
  match t with
  | Nil -> [] 
  | Node(l , x , r ) -> in_order_visit l @ [x] @ in_order_visit r

(* Implémenter une fonction qui trouve les éléments en communs entre deux listes qui sont triès dans un ordre ascendant*)

let rec scan_sorted l1 l2 =
  match l1 , l2 with 
  | [], _ ->  []
  |  _, [] ->  []
  | x :: xs, y :: ys when x = y ->  x :: scan_sorted xs ys
  | x :: xs , y :: ys when x < y ->  scan_sorted xs (y :: ys)
  | x :: xs , y :: ys   ->  scan_sorted (x :: xs)  ys;;


(* Common nodes between two binary search trees*)


let common_nodes t1 t2 =
  let prefix1 = in_order_visit t1 in
  let prefix2 = in_order_visit t2 in
  scan_sorted prefix1 prefix2


(* --- Tests --- *)

let t_empty = Nil

let t_a =
  Node (Node (Node (Nil, 1, Nil), 3, Node (Nil, 4, Nil)),
        5,
        Node (Nil, 7, Node (Nil, 9, Nil)))      (* in‑order: 1 3 4 5 7 9 *)

let t_b =
  Node (Node (Nil, 2, Node (Nil, 3, Nil)),
        5,
        Node (Nil, 7, Node (Nil, 10, Nil)))     (* in‑order: 2 3 5 7 10 *)

let t_c =
  Node (Node (Nil, 11, Nil),
        12,
        Node (Nil, 13, Nil))                    (* disjoint *)


let res = common_nodes t_a t_b;;

let print_list lst =
  List.iter (fun x -> Printf.printf "%d " x) lst;
  print_newline ();;

print_list (in_order_visit t_a);;
print_list (in_order_visit t_b);;


print_list res;;

let () =
  assert (common_nodes t_empty t_empty = []);
  assert (common_nodes t_empty t_a = []);
  assert (common_nodes t_a t_b = [3; 5; 7]);
  assert (common_nodes t_a t_c = []);
  assert (common_nodes t_a t_a = [1; 3; 4; 5; 7; 9]);
  assert (common_nodes t_b t_a = [3; 5; 7]);    (* order of args doesn’t matter *)