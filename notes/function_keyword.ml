(* The function key word here means that the following code is a function that accepts only one parameter
And we are going to directly pattern match on it
it's equivelant to say 
let rec my_func arg = match arg with ...*)
let rec sum = function
    | [] -> 0
    |  x :: t -> x + sum t