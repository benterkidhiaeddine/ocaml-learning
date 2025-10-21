(* 'a list -> 'b  *)

let rec last lst = 
  match lst with
  | [] -> None
  | _ :: t -> last t