let rec rev lst =
  let rec aux lst acc =  
  match lst with
    | [] -> acc
    | x :: t -> aux t (x :: acc)
  in aux lst []


let drop lst n =
  
  snd(  List.fold_left (fun (i, acc) value -> 
    if i >= n  then 
      (i + 1 , acc @ [value])
    else  
      (i + 1 , acc)
  )
  (0, [])
  lst 
  )



let () = assert ( drop [1; 3 ;5 ;6 ;7 ] 3  = [6 ;7 ] )