(* L'ordre n'est pas  important pour les records mais on peut pas utiliser le meme nom deux fois*)
type date = {
  day: int;
  month: string;
  year: int
};;


let today = {
  day = 7;
  month = "december";
  year = 2025
}

type person = {
  nom : string ;
  age : int
}

let e1= {nom="Lucie"; age=17};; 


let person_nom e1 = match  e1 with 
  | {nom; age} -> nom


let () = assert (person_nom e1 = "Lucie");;



(* Examples for patter matching using records*)


type person = { name: string ; age : int }
let is_adult p  = 
  match p with 
    (* Here we can use the usage of syntactic sugar to acess the records values directly using 
      the records field names*)
    | {name ; age } when age > 18 -> true
    | _ -> false

let elisa = { name = "elisa" ; age = 22}


(* We copied the record elisa by changing only the name , we didn't mutate the original record , 
insted we created a new record that has the same values of the one we are copying except a list of definite fields , that we have 
given explicit new values to *)
let elisa_twin_sara = { elisa with name = "sara"} 


let () = assert (is_adult elisa = true)