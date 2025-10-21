let rec count n = 
  if n = 0 then 0 else 1 + count (n - 1)


let a = count 40
let () = Printf.printf "the number is %d\n"  a