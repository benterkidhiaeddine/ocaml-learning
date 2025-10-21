let rec binom n k =
  if n = k ||  k = 0 then 1
  else
    (binom (n - 1) k) + (binom (n-1) (k-1))