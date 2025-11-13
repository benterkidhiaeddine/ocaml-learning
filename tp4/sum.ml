let rec somme_list lst =
  match lst with
    | [] -> 0 
    | x :: t -> x + somme_list t 



let () = assert ( somme_list [1; 2; 3] == 6 ) 
let () = assert ( somme_list [1] == 1 ) 
let () = assert ( somme_list [] == 0 ) ;;
