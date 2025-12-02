let flatten lst =
  List.fold_left 
  (@)
  []
  lst


let () =  assert ( flatten [[1;2;3]; [8;9]; []; [4; 5]] = [1; 2; 3; 8; 9; 4; 5] )