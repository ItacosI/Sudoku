exception OOB;;

(*	Créer une liste de 0 à n
 *	in/ n un entier positif
 *	
 *	out/ une liste d'entier de 0 a n
 *)
let listPos n = 
	let rec listPos_aux k acc =
		match k with
		| 0 -> 0::acc
		| _ -> listPos_aux (k-1) (k::acc) in
	listPos_aux n [];;

(*	Retourne la valeur d'une case d'un tableau
 *  
 *
 *)
let chercheValeurTableau table index =
	let rec cvt tab k =
		match tab with
		| [] -> raise OOB
		| h::t when k=index -> h
		| h::t -> cvt t (k+1) in
	cvt table 0;;

let retireValeurTableau table index =
	let rec rvt tab k acc =
		match tab with
		| [] -> List.rev acc
		| h::t when k = index -> rvt t (k+1) acc
		| h::t -> rvt t (k+1) (h::acc) in
	rvt table 0 [];;

(*	 Creation v2 
 *
 *
 *)
let creation n =
	let table = Array.make_matrix 9 9 '0' in
	let pos = listPos 80 in
    let rec creation_aux k =
    	match k with
    	| 0 -> table
    	| _ -> 
    		let rdm = (Random.int (List.length pos)) in
    		let rdmpos = chercheValeurTableau pos rdm in
			let rdmval = char_of_int (Random.int (9) + 1) in
    		if(table.(rdmpos/9).(rdmpos mod 9) == '0' && test table (rdmpos/9) (rdmpos mod 9) rdmval) then
    			begin
	    			table.(rdmpos/9).(rdmpos mod 9) <- rdmval; 
	    			let pos = retireValeurTableau pos rdm in
	    			creation_aux (k-1)
    			end
    		else
    			creation_aux k in

    creation_aux n;;
