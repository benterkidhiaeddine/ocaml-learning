(* A coin can be a penny, nickel , dime or quarter :
notice the usage of the "or" meaning there is a disjoint union between the set of possibilities *)
type coin = Penny of float | Nickel of float | Dime of float |  Quarter of float 



(* A student  has a name and Id number 
there the usage of the "and"  *)
type student = { name : string  ; id : int  }


(* A dessert has a sauce a creamy componenet and a crunchy component *)
type dessert = { sauce : string  ; crunchy_compnent : string ; creamy_compnent : string}


(*Using vairants we can creating heterogenous lists*)
type int_or_string = 
  | String of string
  | Int of int


let lst: int_or_string list = [String "hello"; Int 8; String "yes"]

(* To print the heterogenous list of element we created a specfic function to do that 
using pattern matching to define the specific behaviour for each type*)
let print_int_or_string el =
  match el with 
    | String s -> Printf.printf "%s \n" s
    | Int i -> Printf.printf "%d \n" i
