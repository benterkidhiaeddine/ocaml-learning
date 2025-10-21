(* p is fib n - 1 and pp is fib n - 2*)
let fib_fast n = 
  let rec h n pp p =
    if n = 1 then p
    else
      h (n -1) p  (pp + p)
    in 
  if n = 0 then 0
  else
    h n 0 1;;


print_int (fib_fast 0);


(* pour trouver l'overflow il faut aller faire une boucle pour chercher 
un nombre de n suffisament grand pour qu'il produit un nombre négatif*)

let search_neg_fib () =
  let rec search_aux acc =
    if fib_fast acc < 0 then acc
    else search_aux (acc + 1)
  in
  search_aux 0;

print_int (search_neg_fib)