type nucleotide = A | C | G | T

let rec hamming_distance dna1 dna2 =
  if List.length dna1 <> List.length dna2 then
    Error "strands must be of equal length"
  else
  let rec aux dna1 dna2  acc = 
  match dna1 , dna2 with
    | [], [] -> Ok acc
    | x :: xs, y :: ys -> if x <> y then  aux xs ys   (acc + 1) else aux xs ys (acc)
    | _ -> failwith "strands must be of equal length"
  in aux dna1 dna2 0

