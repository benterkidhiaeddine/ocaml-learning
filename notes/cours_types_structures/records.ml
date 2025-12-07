(* L'ordre n'est importants pour les records mais on peut pas utiliser le meme nom deux fois*)
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