(*	Affiche la table du sudoku 
 *	src -> matrix 9*9
 *	0 -> affiche blanc
 *	_ -> affiche chiffre
 *)
	let afficheTable src =
		let affLigne l =
			Printf.printf "|%c%c%c|%c%c%c|%c%c%c|\n" l.(0) l.(1) l.(2) l.(3) l.(4) l.(5) l.(6) l.(7) l.(8) in
		
		Printf.printf "-------------\n";
		affLigne (src.(0));
		affLigne (src.(1));
		affLigne (src.(2));
		Printf.printf "-------------\n";
		affLigne (src.(3));
		affLigne (src.(4));
		affLigne (src.(5));
		Printf.printf "-------------\n";
		affLigne (src.(6));
		affLigne (src.(7));
		affLigne (src.(8));
		Printf.printf "-------------\n";;
		
		
let transfo s =
  let table = Array.make_matrix 9 9 '0' in
  for k = 0 to 80 do
    table.(k/9).(k mod 9) <- (s.[k]);
  done;
  table;;
  
afficheTable (transfo "790304108000006000000080059030000497000000000217000030470010000000700000302609071");;

