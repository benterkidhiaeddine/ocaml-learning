let rec interleave lst1 lst2 =
  match lst1, lst2 with 
    | [], [] -> []
    | [], _ -> lst2
    | _, []->  lst1
    | x1 :: t1, x2 :: t2 ->  x1 :: x2 :: interleave t1 t2


let () = assert (interleave  [1 ; 2 ; 3] [100 ; 200 ; 300] =  [1; 100; 2; 200; 3; 300])
let () = assert (interleave  [1 ] [100 ; 200 ; 300] =  [1; 100; 200; 300])
let () = assert (interleave  [1 ; 2 ; 3] [100] =  [1; 100; 2; 3])
