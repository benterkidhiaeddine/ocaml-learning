(* Implement the preorder tree-walk algorithm
Visit the value in the node first than it's left subtree then it's right subtree*)
type t = Node of t * int * t | Nil


let rec pre_order_traversal t  =
  match t with 
  | Nil -> []
  | Node(l, x , r) -> [x] @
    pre_order_traversal l  @ 
    pre_order_traversal r ;;

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

let res = pre_order_traversal t;;
List.fold_left (fun i x -> Printf.printf "%d : %d \n" i x ; i + 1) 0 res;;  