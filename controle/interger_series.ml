let rec interger_series n =
  let rec aux n a =
    if n = 0 then [] 
    else  a ::  aux (n - 1) (a + 1)  
  in aux n 1;;


let () = assert ( interger_series 5 = [1;2;3;4;5]  )