let rec nth_digit i x  = 
if x <= 0 then 0
else if i = 1 then (x mod 10)
else nth_digit (i - 1) (x / 10)
