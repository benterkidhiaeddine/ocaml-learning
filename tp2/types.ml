(* bool -> bool*)
let f x = if x then x else x
(* 'a -> bool -> 'a*)
let g x y = if y then x else x
(* bool -> 'a -> 'a -> 'a *)
let h x y z = if x then y else z
(* bool -> 'a -> 'b -> 'a*)
let i x y z = if x then y else y