let sum lst =
  List.fold_left  (+) 0 lst


let () = assert ( sum [1; 2 ;3] = 6)