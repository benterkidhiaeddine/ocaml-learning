(* Défintion d'un arbre binaire*)
type t =  Node of t * int * t | Nil
(* Trouver tous les feuilles de l'arbre *)
let rec feuilles t = 
  match t with  
  | Nil -> []
  | Node(Nil , x , Nil ) -> [x]
  | Node(l, x , r) -> feuilles l @ feuilles r

let t = Node(
  Node(
    Node(Nil, 1,Nil) ,
     2 ,
      Node(Nil, 5, Nil)
      )
  , 7
  ,Node(
    Node(Nil, 8 , Nil)
    , 10 
    , Node(Nil, 11, Nil)
  )
);;


let t2 = Node(
  Node(
    Node(Nil, 2,Nil) ,
     7 ,
      Nil
      )
  , 8
  ,Node(
    Node(Nil, 9 , Nil)
    , 10 
    , Node(Nil, 13, Nil)
  )
);;





(* Test the feuilles function*)
let () = assert (feuilles t = [1;5;8;11] )


let rec find el t =
  match t with
  | Nil -> false
  | Node(l , x , r) -> x = el || find el l || find el r

(* Test the find function*)

let () = assert (find 5 t = true)
let () = assert (find 22 t = false)



let rec impass t =
  let feuiles_t = feuilles t in 
  let rec aux lst =
    match lst with 
    | [] -> false
    | x ::  xs ->if x = 1 then (find 2 t)  || aux xs
                 else  (find (x + 1) t && find (x - 1) t) || aux xs 
  in 
  aux feuiles_t
 


(* Test the impass function*)

let () = assert (impass t = true) 
let () = assert (impass t2 = true)