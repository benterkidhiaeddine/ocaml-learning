
let rec fold_left f init lst = 
  match lst with
    | [] -> init
    | x :: t ->  fold_left f (f init x) t 

let rec rev lst =
  let rec aux lst acc =  
  match lst with
    | [] -> acc
    | x :: t -> aux t (x :: acc)
  in aux lst []


let () = assert ( rev [1; 2; 3 ;4] = [4; 3; 2; 1])


let slice start stop lst = 
  let (_, res) = fold_left (fun (i , acc) x -> 
    if i >= start && i < stop then
     (i + 1 , x :: acc) 
    else
    (i + 1 , acc)
  ) 
  (0, []) 
  lst
  in
  rev  res


