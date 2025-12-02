let contains lst num = 
  List.fold_left (fun acc value -> 
      acc || (value = num)
      ) 
      false
      lst
    
let () = assert ( contains [1 ; 2; 0 ;4] 4 = true)
let () = assert ( contains [1 ; 2; 8 ;4] 12 = false)