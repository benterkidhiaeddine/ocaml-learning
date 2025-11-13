let is_divisor m n = m mod n = 0
let rec est_quadratfrei n =
  let rec aux m n =
    if m * m > n then true
    else
    not (is_divisor (m * m) n) && (aux (m + 1) n )
    in 
    aux 2 n
;;

let () = assert (est_quadratfrei 30 = true)  