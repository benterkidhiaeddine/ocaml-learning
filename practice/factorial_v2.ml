(* La version recursive terminal de la fonction terminal *)
(* cetter version est plus optimisé que la version recursive normal*)
let rec fact_aux n acc = 
  if n = 0 then acc
    else fact_aux (n-1) (acc * n)



(*L'accumalateur doit etre l'élément neutre par rapport à l'opération de la récursion 
dans ce cas l'opération c'est la multiplication donc l'élément neutre c'est 1*)
let rec fact_tr n = fact_aux n 1

let fact_0  = fact_tr 0
let fact_1  = fact_tr 1
let fact_4  = fact_tr 4
let fact_5  = fact_tr 5
let fact_6  = fact_tr 6

let () = Printf.printf "the result of 0 factorial is %d\n" fact_0
let () = Printf.printf "the result of 1 factorial is %d\n" fact_1
let () = Printf.printf "the result of 4 factorial is %d\n" fact_4
let () = Printf.printf "the result of 5 factorial is %d\n" fact_5
let () = Printf.printf "the result of 6 factorial is %d\n" fact_6


(*Dans cette version la focntion auxilliaire est scopé localement dans la défintion de la focntion
global , c'est une solution plus élegante*)
let rec fact_tr_elegant n =
  let rec fact_aux n aux =
    if n = 0 then aux
    else ((print_int aux) ; (print_endline "") ; fact_aux (n - 1) (aux * n)) in
  fact_aux n 1

let res_final = fact_tr_elegant 4