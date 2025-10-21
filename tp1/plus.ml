let rec plus a b = if  b = 0 then a 
else  1 + plus a (b-1)



let rec prod a b = if  b = 0 then 0 
else  a + prod a (b-1)