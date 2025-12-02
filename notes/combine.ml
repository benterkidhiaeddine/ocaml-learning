(* We are going to try to construct the idea of the fold function progressively
by extracting the common idea behind combining elements in a list *)


(*to summ all the elements of a list*) 
let rec sum = function
  | [] -> 0
  | x :: t ->  x + sum t


(* to concat all the elements of a list *) 
let rec concat = function
  | [] -> ""
  | x :: t ->  x ^ concat t

(* We notice that there a is a repition between the two functions 
1. We are using an initial value that we are adding into each time 0 in the first 
one and the empty string in the second one so we are going to call it init
2. We are using a combination operator to combine it with the recursive call
on the rest of the elements of the list*)

(* Let's define the function combine*)
let rec combine init op lst = 
  match lst with 
    | [] -> init
    | x :: t -> op x (combine init op lst)


(* What we obtain is actualy the library function fold_right*)

let rec fold_right f lst (acc : 'acc) =
  match lst with
  | [] -> acc
  | x :: t -> f x (fold_right f lst acc)

