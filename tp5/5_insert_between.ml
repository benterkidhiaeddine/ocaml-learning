

let insert_between num lst =
  match lst with
    | [] -> []
    | [x] -> [x]
    | x :: t ->  List.fold_left  (fun acc value -> 
    acc @ [num ; value ])
    [x]
    t
   
  ;; 
open Printf
let a =  insert_between 100 [2; 4 ;5 ]
let () = List.iter (printf "%d ") a
let () = assert ( insert_between 100 [2; 4 ;5 ] = [2;  100 ;4 ; 100 ;5])