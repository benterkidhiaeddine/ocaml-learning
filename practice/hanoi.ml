let move_disk origine destination =
  print_string "Deplacer le disque du pilier ";
  print_int origine;
  print_string " vers le pillier ";
  print_int destination;
  print_endline ".";;


let rec hanoi origine destination  aide disques =
  if disques = 1  then move_disk origine destination 
  else
    begin 
      hanoi origine aide destination (disques - 1);
      move_disk origine destination;
      hanoi aide destination origine (disques - 1)
    end;;



hanoi 1 3 2 2;
print_endline "for 3";
hanoi 1 3 2 3;
print_endline " for 4";
hanoi 1 3 2 4


