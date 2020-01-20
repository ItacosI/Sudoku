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
