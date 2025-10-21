let rec count l a = 
  if l = [] then 0
  else if (List.hd l) = a then
     (1 + (count (List.tl l) a)) 
  else count (List.tl l) a


let () = assert (count [2 ; 5 ; 5 ;6 ;7] 5 = 2)
let () = assert (count [] 0 = 0)
let () = assert (count [5 ; 5 ; 5 ;5 ] 2 = 0)


(* Pattern matching version *)
let rec count_v2 l a = 
  match l with 
  | [] -> 0
  | h :: t -> if h = a then
    1 + count t a
    else
      count t a  


let () = assert (count_v2 [2 ; 5 ; 5 ;6 ;7] 5 = 2)
let () = assert (count_v2 [] 0 = 0)
let () = assert (count_v2 [5 ; 5 ; 5 ;5 ] 2 = 0)

