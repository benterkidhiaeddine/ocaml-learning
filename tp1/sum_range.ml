let rec sum_range a b =
  if a = b then a
  else b + sum_range a (b - 1)