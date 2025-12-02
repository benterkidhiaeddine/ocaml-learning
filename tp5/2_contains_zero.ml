let contains_zero lst = 
  List.fold_left (fun acc value  -> 
      acc || (value = 0)
      ) false  lst
    
let () = assert ( contains_zero [1 ; 2; 0 ;4] = true)
let () = assert ( contains_zero [1 ; 2; 8 ;4] = false)