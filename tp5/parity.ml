(*Fonction map prédefinie*)
let rec map f lst = 
  match lst with
    | [] -> []
    | x :: t -> f x :: (map f t)


let parity lst = 
  map (fun x -> x mod 2 <> 0) lst


let () = assert (  parity [1; 2; 3; 4; 5] =  [true; false; true; false; true] )