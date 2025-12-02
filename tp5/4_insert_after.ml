let insert_after num lst  =
  List.fold_right (fun value acc -> 
    value :: num :: acc)
    lst 
    [] 



let () = assert (insert_after 100 [2; 4 ;5 ] = [2; 100 ;4 ;100 ;5 ;100])