(*Ouverture de fichier et lecture : close_in ? le i = 'numero de fichier', s ="grids/grid" ou "solutions/solution" *)
let l i s = 
  let l1 = (s ^ Char.escaped i) in 
  let l2 = ( l1^ Char.escaped '.') in 
  let l3  = (l2^ Char.escaped 't')in 
  let l4  = (l3^ Char.escaped 'x') in 
  let l5 = (l4^ Char.escaped 't') in 
  let c = open_in l5 in 
  input_line c

(*	Retire nb chiffres de la table table et les remplaces par le charactere zéro
 *
 * 	in/ nb un entier compris entre 0 (pas d'effet) et le nombre max de valeur
 *		différentes de zéro
 *	in/ une table contenant l'ensemble
 *)


let retireChiffres nb table =
	let rec rc k =
		match k with
		| 0 -> ();
		| _ -> 
			let i = (Random.int (9)) in
			let j = (Random.int (9)) in
			match table.(i).(j) with
				| '0' -> rc k
				| _ -> table.(i).(j) <- '0'; rc (k-1) in

	rc nb;;





(*Test le joueur a place le chiffre au bon endroit
maJ -> matrice du joueur
maS -> matrice solution 
i , j -> coordonnée de la case a tester*)
let testSiNombreCorrect maJ maS i j = if maJ.(i).(j) == maS.(i).(j) then true else false




(*Met un nombre correcte dans la case de la matrice du joueur
maJ -> matrice du joueur
maS -> matrice solution 
i , j -> coordonnée de la case a tester*)
let aideJoueurSurUneCaseDonnee maJ maS i j = maJ.(i).(j) <- maS.(i).(j) 
