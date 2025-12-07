type color = Blanc | Noir | Vert | Rouge;;


let inverse_color c = 
  match c with 
  | Blanc -> Noir
  | Noir -> Blanc
  | Vert -> Rouge
  | Rouge -> Vert
  