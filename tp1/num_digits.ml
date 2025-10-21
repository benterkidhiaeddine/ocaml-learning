let rec num_digits x = if x < 10 && x > (-10) then 1
else 1 + num_digits(x / 10)
