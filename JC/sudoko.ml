
(* On crée un type grille *)
type grille = bool array array;;



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

    exception Faux;;

(*Test si le nombre placé n'est pas deja present dans la colonne *)

let testCol ma i j va =
  let rec test1 k =
  if k >= 9 then true else
  begin
    if ma.(k).(j) == va then raise Faux else test1 (k + 1)
    end
      in try test1 0 with Faux -> false




(*Test si le nombre place n'est pas deja present dans la ligne *)

let testLig ma i j va =
  let rec test1 k =
    if k >= 9 then true else
      begin
        if ma.(i).(k) == va then raise Faux else test1 (k + 1)
      end
      in try test1 0 with Faux -> false





(*recherche la zone 0 ou 1 ou 2 *)

  let rechercheCadre k =
   if k>=0 && k < 3 then 0 else
    begin
      if k >=3 && k < 6 then 3 else 6
    end






(*Test si le nombre place n'est pas deja present dans la région*)

    let cadre ma i j va =
    try
    let posi = rechercheCadre i in let posj = rechercheCadre j in
      for x =  posi to (posi + 2) do
        for  y = posj to (posj + 2) do
            (Printf.printf "%d %d %c\n" y x ma.(x).(y) ;
            if ma.(x).(y) = va then raise Faux else () )
        done
      done;
      true
      with Faux -> false




let test ma i j va = cadre ma i j va && testLig ma i j va && testCol ma i j va


let testFinal ma =
  let res = ref true in
  for i = 0 to 8 do
    for j = 0 to 8 do
      res := (!res && test ma i j ma.(i).(j))
    done
  done ; !res

(* On crée une fonction qui prend en paramètre une chaine de caractère(qui représentera le sudoku en brut) et renvoie une matrice 9*9 *)

let transfo s =
  let table = Array.make_matrix 9 9 '0' in
  for k = 0 to 80 do
    table.(k/9).(k mod 9) <- (s.[k]);
  done;
  table;;

let testSiNombreCorrect maJ maS i j = if maJ.(i).(j) == maS.(i).(j) then true else false;;


let compteur = ref 81;;

let vie = 10;;
(* Fonction qui va ajouter à la matrice la valeur donnée en paramètre si cela est possible *)

let ajout ma i j va =

  if(ma.(i).(j) == '0' && test ma i j (char_of_int(va+48))) then
  begin
  ma.(i).(j) <- (char_of_int (va+48)); !compteur++
end

else  begin
  if (test ma i j (char_of_int(va+48)) && not(testSiNombreCorrect ma maS i j )) then
    begin
      ma.(i).(j) <- (char_of_int (va+48));
      if (testSiNombreCorrect ma maS i j ) then
        !vie--
    end

end





(* On crée une fonction création qui va créer une grille de taille 9 * 9 avec des valeurs déjà créées aléaoirement et ce si c'est possible *)

    let creation n =
      let table = Array.make_matrix 9 9 '0' in
      let rec aux k =
          Printf.printf "%d\n" k;
          if k == 0 then table else begin
          let valrandomi = (Random.int (9)) in
          let valrandomj = (Random.int (9)) in

          let valrandom = char_of_int (Random.int (9)+49) in

          if (table.(valrandomi).(valrandomj) == '0' && (test table valrandomi valrandomj valrandom)) then

           begin
              (table.(valrandomi).(valrandomj) <- valrandom); aux (k-1)
            end
          else aux k

        end
          in aux n;;
