(* Défintion de type arbre binaire *)
type t = Node of t * int * t | Nil


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



(* La première étape est d'applatir l'arbre binaire en utilisant un algorithme de visite 
in order , comme ca on va avoir la liste valeurs de plus petit au plus grand *)


let rec rev_in_order_traversal = function
  | Nil -> []
  | Node(l , x , r) -> rev_in_order_traversal r @ [x] @ rev_in_order_traversal l

(* Reverse a list algorithm*)
let rec rev lst = 
  let rec aux acc lst =
    match lst with
    | [] ->  acc
    | x :: t ->  aux ( x :: acc ) t
  in 
  aux [] lst

(* nth element in List*)
let rec nth n l =
  match l with 
    | x :: t -> if n = 0 then Some x else nth (n - 1) t
    | [] -> None 
let kth_greatest_element  t k =
  nth (k - 1)  (rev_in_order_traversal t );;
 
 
  
let () = assert (kth_greatest_element t 1= Some 11 ) 