let rec map f lst = 
  match lst with
    | [] -> []
    | x :: t -> f x :: (map f t)

let () = assert ( map (fun x -> x + 1) [1;2;3;4] = [2;3;4;5] )


