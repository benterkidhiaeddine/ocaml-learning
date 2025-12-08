(* the built in variant data type. Option stands for either There  is Somthing of a given type or Nothing*)

let devide a b =
  match a , b with
    | a, 0 -> None
    | a, b -> Some (a / b);;



devide 4 0;;


(* Get values , and if there is nothing set a default value depending on the context*)
let get_val default = function
  | None -> default 
  | Some x -> x;;


print_int (get_val  0 (Some 5));;


(* Example of usage of options is when trying to get a specific value from a list *)
(* The example of the max_list function , in the case for an empty list we need to return None else Some max*)
let rec max_list = function
  | [] -> None
  | h :: t -> begin match max_list t with
    | None -> Some h 
    | Some m -> Some (max h m)
    end

let () = assert (max_list [] = None)
let () = assert (max_list [1 ; 2 ; 3] = Some 3);;

