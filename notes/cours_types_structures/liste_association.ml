let bureaux = [ ("pierre", 101); ("julie", 103); ("paul", 205) ];;


let rec trouver nom liste =
  match liste with
    | [] -> failwith ("Nom␣pas␣trouvee␣:"^nom)
    | (c,v)::r when c = nom -> v
    | _::r -> trouver nom r;;
  

(* Return the value assocaited with the key in a associative list ( aka list of tuples)*)
let () = assert (List.assoc "pierre" bureaux = 101) ;;

(* Same as assoc but only returns True or False depedning if the key key exists or not*)
let () = assert (List.mem_assoc "james" bureaux = false) ;;