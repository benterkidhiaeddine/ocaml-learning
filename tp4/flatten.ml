let rec flatten lst1  =
  match lst1 with
    | [] -> []
    | x :: t -> x @ flatten t


let () = assert (flatten  [[1;2;3]; [8;9]; []; [4; 5]] = [1; 2; 3; 8; 9; 4; 5])
