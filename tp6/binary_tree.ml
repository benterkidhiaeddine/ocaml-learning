type 'a bintree = 
  | Vide
  | Node of 'a * 'a bintree * 'a bintree;; 

(* Function that verifies if a binary tree is a search binary tree*)

let node1_less_than_node2 n1 n2 =
  match n1, n2 with 
  | Vide , Vide -> true
  | Vide, _ -> true
  | _ , Vide -> true
  | Node (num1, left1, right1 ), Node (num2, left2, right2) -> num1 < num2
let rec is_BST t =
  match t with
  | Vide -> true
  | Node (num , left ,right) ->   node1_less_than_node2 left t
    && node1_less_than_node2 t right
    && is_BST left 
    && is_BST right

let example_tree = Node(7 , 
                    Node(3,   
                        Node(1, Vide, Vide) ,
                        Node(5, Vide , Vide)),
                    Node(10 , 
                        Node(8 ,Vide, Vide),
                        Node(11, Vide, Vide))
)

let () = assert (is_BST example_tree = true);;

(* Prof solution *)
(* First implement the function that makes sure the ordering of the numbers in the nodes is correct *)


type t = Node of t * int * t | Nil


(* This function takes a comparaison function as input as well as a number
it makes sure alle values of the sub tree satisfy the comparison function in relation
to the inputed value
So in our case all values of the left subt tree must be less then the inputed value
And all the values of the irght sub tree must be greater than the inputed value*)
let rec for_all p = function
  | Nil -> true
  | Node(l,x,r) -> p x (*&& for_all p l && for_all p r*)


(* To test that a binary tree is a binary search tree all values on the left need 
to be less than or equal the current node value , all values on the right must be greater or equal to the current value
the left subtree must be also a binary search tree ,as well as the right sub tree *)
let rec is_search_tree = function
  | Nil -> true
  | Node(l,x,r) ->
    for_all (fun y -> y <= x) l && for_all (fun y -> y >= x) r &&
    is_search_tree l && is_search_tree r

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
)


let () = assert ( is_search_tree t = true);;