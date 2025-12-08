(* Implement the post tree-walk algorithm
Visit the value in the   left subtree then it's right subtree than in the node*)
type t = Node of t * int * t | Nil


let rec post_order_traversal t  =
  match t with 
  | Nil ->  []
  | Node(l, x , r) -> 
    post_order_traversal l  @ 
    post_order_traversal r  @
    [x]

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

let res = post_order_traversal t;;

List.fold_left (fun i x -> Printf.printf "%d : %d \n" i x ; i + 1) 0 res;;  
