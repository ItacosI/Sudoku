(*ocaml graphics.cma -init graph.ml*)

(*ocamlc -o exo graphics.cma graph.ml*)


(* let affiche_tableau_couleur(m,nb_ligne,nb_colonne) =

    for  i=0 to (nb_ligne-1) do
      for j=0 to (nb_colonne-1) do

    begin

    match m.(i).(j) with
    |'1' -> set_color (red);
         fill_circle (i*20) ((nb_colonne-j)*20+480) 10;
                        (*     (i*20+25, j*20+480, 10);*)

    |'0' -> Graphics.set_color (black);
        fill_circle (i*20) ((nb_colonne-j)*20+480) 10 ;
         (*(i*20+25, j*20+480, 10);*)

    end
      done;
      print_newline();
    done
   ;; *)


let compteur = ref 81;;

let vie = ref 10;;

(* Ouvre et renvoie le contenu du fichier "solutions/solution[i].txt"
 *
 *  i le numéro du fichier
 *
*)
let lecturesolution i =
  let l5 = String.concat "" ["solutions/solution"; string_of_int i; ".txt"] in
  let c = open_in l5 in
  input_line c;;

let lecturegrid i =
  (* let l5 = String.concat "" ["grids/grid"; string_of_int i; ".txt"] in *)
  let l5 = "grids/grid" ^ (string_of_int (i)) ^ ".txt" in

  let c = open_in l5 in
  input_line c;;



(*  Retire nb chiffres de la table table et les remplaces par le charactere zéro
 *
 *  nb un entier compris entre 0 (pas d'effet) et le nombre max de valeur
 *    différentes de zéro
 *  table une matrice contenant le sudoku à modifier
 *
*)



let retireChiffres nb table =
  let rec rc k =
    match k with
    | 0 -> ();
    | _ ->
      let i = (Random.int (81)) in
      match table.(i) with
      | 0 -> rc k
      | _ -> table.(i) <- 0; rc (k-1) in
  rc nb;;



(*  Test le joueur a place le chiffre au bon endroit
 *  maJ -> matrice du joueur
 *  maS -> matrice solution
 *  i , j -> coordonnée de la case a tester
 *
*)
let testSiNombreCorrect maJ maS i j = if maJ.(i).(j) == maS.(i).(j) then true else false;;


let test2 x y = Printf.printf"test";;

(*  Met un nombre correcte dans la case de la matrice du joueur
 *  maJ -> matrice du joueur
 *  maS -> matrice solution
 *  i , j -> coordonnée de la case a tester
 *
*)
let aideJoueurSurUneCaseDonnee maJ maS i j = maJ.(i).(j) <- maS.(i).(j);;



(*	Affiche la table du sudoku
 *
 *	src -> matrix 9*9
 *	0 -> affiche blanc
 *	_ -> affiche chiffre
 *
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


(*  Test si le nombre placé n'est pas deja present dans la colonne
 *
 *  ma la matrice contenant le sudoku
 *  j la position de la colone à tester
 *  va la valeur pour laquelle on mene le test
 *
*)
let testCol ma j va =
  let rec test1 k =
    if k >= 9 then true else
      begin
        if ma.(k).(j) == va then raise Faux else test1 (k + 1)
      end
  in try test1 0 with Faux -> false;;




(*  Test si le nombre place n'est pas deja present dans la ligne
 *
 *  ma la matrice contenant le sudoku
 *  i la position de la ligne à testet
 *  va la valeur pour laquelle on mene le test
 *
*)

let testLig ma i va =
  let rec test1 k =
    if k >= 9 then true else
      begin
        if ma.(i).(k) == va then raise Faux else test1 (k + 1)
      end
  in try test1 0 with Faux -> false;;




(*  Recherche la region pour une ligne ou une colonne donnée
*)

let rechercheCadre k =
  if k>=0 && k < 3 then 0 else
    begin
      if k >=3 && k < 6 then 3 else 6
    end;;






(*Test si le nombre place n'est pas deja present dans la région*)

let cadre ma i j va =
  try
    let posi = rechercheCadre i in let posj = rechercheCadre j in
    for x =  posi to (posi + 2) do
      for  y = posj to (posj + 2) do
        (* (Printf.printf "%d %d %c\n" y x ma.(x).(y) ; *)
        if ma.(x).(y) = va then raise Faux else ()
      done
    done;
    true
  with Faux -> false;;




let test ma i j va = (cadre ma i j va) && (testLig ma i va) && (testCol ma j va);;

(*
let testFinal ma =
  let res = ref true in
  for i = 0 to 8 do
    for j = 0 to 8 do
      res := (!res && test ma i) j ma.(i).(j))
    done
  done ; !res;; *)

(* On crée une fonction qui prend en paramètre une chaine de caractère(qui représentera le sudoku en brut) et renvoie une matrice 9*9 *)

let transfo s =
  let table = Array.make_matrix 9 9 '0' in
  for k = 0 to 80 do
    table.(k/9).(k mod 9) <- (s.[k]);
  done;
  table;;


let testSiNombreCorrect va maS i j = if va == maS.(i).(j) then true else false;;




let copy src =
  let res = Array.make_matrix 9 9 '0' in
  for i = 0 to 8 do
    res.(i) <- Array.copy src.(i)
  done;
  res;;


(* Fonction qui va ajouter à la matrice la valeur donnée en paramètre si cela est possible *)

let ajout ma maS i j va =
  match ma.(i).(j) with
  | '0' when (testSiNombreCorrect (char_of_int (va+48)) maS i j) -> ((ma.(i).(j) <- (char_of_int (va+48))); compteur := !compteur+1);
  | '0' -> vie := !vie-1;
  | _ -> ();;





(* On crée une fonction création qui va créer une grille de taille 9 * 9 avec des valeurs déjà créées aléaoirement et ce si c'est possible *)

let creation n =
  let table = Array.make_matrix 9 9 '0' in
  let rec aux k =
    (* Printf.printf "%d\n" k; *)
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





let procedurealeatoire arraygrille arraysolution =
  let valrandomgrille = ref (Random.int (81)) in 
  let val1 = ref 0 in
  let bool = ref true in


  while (!bool) do
    if((arraygrille.(!valrandomgrille) = 0 ) && arraysolution.(!valrandomgrille) != 0)
    then ((arraygrille.(!valrandomgrille) <- arraysolution.(!valrandomgrille)); val1 := arraysolution.(!valrandomgrille);bool := false )
    else (valrandomgrille := ((!valrandomgrille + 1 ) mod 81 ))

  done; (!valrandomgrille)





(* Fonction de tour global *)
(* let main =

   Random.self_init ();

   Printf.printf "Quelle grille voulez-vous (0 - 243) : ";
   let n = read_int () in
   let maS = transfo (lecture n) in
   let maJ = copy maS in
   Printf.printf "\nDifficulté (0 - 40) : ";
   let dif = read_int () in
   retireChiffres (61 - dif) maJ;

   while !compteur<=80 do
    afficheTable maJ;
    Printf.printf "Avancement : %d/81 | Vie : %d\n" !compteur !vie;
    Printf.printf "Ligne   : ";
    let i = read_int () in
    Printf.printf "Colonne : ";
    let j = read_int () in
    Printf.printf "Valeur  : ";
    let v = read_int () in
    ajout maJ maS i j v;
   done;; *)
