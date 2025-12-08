type point = float * float
type vector = float list
type matrix = float list list


(* Calculate euclidian distance between two points*)


let euclidian_distance p1 p2 =
  sqrt((fst(p2) -. fst(p1)) ** 2. +. (snd(p1) -. snd(p2)) ** 2.);;


print_float (euclidian_distance (1. ,1.) (2., 2.))
 