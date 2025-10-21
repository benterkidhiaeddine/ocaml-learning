let rec pgcd a b = 
  if a = b then a
  else if a > b then 
    pgcd (a - (a mod b) ) b
  else  
    pgcd a (b - (a mod b));;



print_int (pgcd 3 27)