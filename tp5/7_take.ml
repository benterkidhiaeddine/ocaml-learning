let rec rev lst =
  let rec aux lst acc =  
  match lst with
    | [] -> acc
    | x :: t -> aux t (x :: acc)
  in aux lst []


let take lst n =
  let (_ , r ) =  List.fold_left (fun (i, acc) value -> 
    if i < n  then 
      (i + 1 , value :: acc)
    else  
      (i + 1 , acc)
  )
  (0, [])
  lst 
  in rev r



let () = assert ( take [1; 3 ;5 ;6 ;7 ] 3  = [1; 3 ;5 ] )