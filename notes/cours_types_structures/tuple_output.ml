(* This function give a tuple with two lists the first one contains number less than the pivot *)
(* The second list contains all numbers greater or equal than the pivot*)
let rec split pivot liste = 
  match liste with
  | [] -> ([], [])
  | h::r ->
      let (r1,r2) = split pivot r
      in if h < pivot then
          (h::r1,r2)
         else (r1,h::r2);;

let () = assert (split 17 [4;67;1;13;25] = ([4; 1; 13], [67; 25]));;