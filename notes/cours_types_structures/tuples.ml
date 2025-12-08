(* Deconstruction of tuples functions*)
let fst z =
   let (x,y) = z 
  in x;;

let snd z =
   let (x,y) = z 
  in y;;

(* Example of tuple defintions*)
type point = float * float;;

let a : point = (1.5 , 2.5);;
