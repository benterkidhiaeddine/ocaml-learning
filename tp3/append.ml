(* Appending two lists is connecting the end of the first lst1 to the beggining of the 
second list lst2 
example :
  append [1 ; 2 ;3 ] [4 ; 5]  will produce [1; 2 ;3 ;4 ;5]*)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: append t lst2 


let () = assert (append [4 ; 5 ; 6 ] [7] = [4 ; 5 ;6 ;7])
let () = assert (append [] [7] = [7])
let () = assert (append [1] [7 ; 6 ; 5] = [1 ; 7 ; 6 ;5])