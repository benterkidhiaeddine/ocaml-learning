(* pp denotes the previous previous number in the series and p the previous number *)
let wierd_fib_series n =
  let rec aux n pp p =
    if n = 0 then []
    else 
      pp :: aux (n - 1) p (pp * p)

  in aux n 2 3;;

wierd_fib_series 5;;
let () = assert (wierd_fib_series 5  = [2;3;6;18;108])