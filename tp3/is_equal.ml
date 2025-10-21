(* Ecrire une fonction is_equal l c'est tous les éléments de la liste sont le memes *)
let rec is_equal l =  
  match l with
    | [] -> true
    | [_]  -> true
    | h :: x :: t  -> h = x && is_equal (x :: t)

let () =  assert (is_equal [1; 1 ;1] = true)
let () =  assert (is_equal [1; 2 ;1] = false)
let () =  assert (is_equal [] = true)
let () =  assert (is_equal [2] = true)
let () =  assert (is_equal [1; 1 ; 5 ;5 ;5; 5] = false)