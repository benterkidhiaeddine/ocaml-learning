(*Fonction map prédefinie*)
let rec map f lst = 
  match lst with
    | [] -> []
    | x :: t -> f x :: (map f t)

let increment_all lst = 
  map (fun x -> x + 1) lst

let () = assert ( increment_all [1;2;3;4] = [2;3;4;5] )