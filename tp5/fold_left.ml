(* Cette fonction elle est récursive terminal il n y a pas de travail qui reste à faire après l'appel récursive *)
let rec fold_left f init lst = 
  match lst with
    | [] -> init
    | x :: t ->  fold_left f (f x init) t 


let () = assert ( fold_left (+) 2 [1;2;3]  = 8 )
let () = assert (fold_left (-) 0 [1;2;3] = 2)