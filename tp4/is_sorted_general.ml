(*Manually defined the xor operator to allow for the inversing of the sorting order when rev is set to true*)
let (^^) a b = (a && not b) || (not a && b);;



let less_than x y =
  x < y


let greater_than x y =
  x > y

let rec is_sorted cmp rev lst = 
  match lst with 
    | [] -> true
    | [_] -> true
    | x :: y :: t  -> (cmp x y ^^ rev)  && is_sorted cmp rev(y :: t) 


  
let () = assert (is_sorted less_than false [1; 5 ; 9; 12 ; 20] == true)
let () = assert (is_sorted less_than true [1; 5 ;  6 ;7; 10] == false)
let () = assert (is_sorted less_than true [10; 7 ;6 ;5 ;1] == true)
let () = assert (is_sorted greater_than false [10; 7 ;6 ;5 ;1] == true)
let () = assert (is_sorted greater_than true [10; 7 ;6 ;5 ;1] == false)
let () = assert (is_sorted less_than false [] == true)
let () = assert (is_sorted greater_than true [22] == true)