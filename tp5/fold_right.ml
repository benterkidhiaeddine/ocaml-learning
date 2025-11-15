(* cette fonction n'est pas récursive terminal parfois il est intéressant de reverser la liste et après faire un fold left pour
utiliser la propriété récursive terminal de cette dernière *)

let rec fold_right f lst init = 
  match lst with
    | [] -> init
    | x :: t ->  f x ( fold_right f t init)


let () = assert ( fold_right (+) [1;2;3] 2 = 8 )
let () = assert ( fold_right (-) [1;2;3] 0  = -6) 