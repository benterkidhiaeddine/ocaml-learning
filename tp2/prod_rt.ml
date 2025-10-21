(*Il faut utiliser la version récursive terminal  de plus pour 
que la fonction prod soit une fonction récursive terminal*)

let rec plus_rt a b =
  if b = 0 then a
  else plus_rt (a + 1) (b - 1)
  
let rec prod_rt a b = 
  let rec prod_aux a b acc =
    if b = 1 then acc
    else
      prod_aux a (b - 1 ) (plus_rt acc a) in
  prod_aux a b a;;


  
print_int (prod_rt 25 5);
