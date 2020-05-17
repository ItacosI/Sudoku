
open Graphics;;
let height = 700;;


(* On open la partie graphique en spécifiant la résolution *)
open_graph " 1280x720";;



Random.self_init ();;


(* Fonction permettant de load l'immage yol.ppm *)
let menuDebut bool=


  if(bool = true) then
    (draw_image(Image.init_image "yol.ppm") 0 0) else ();;


(* Variables réference du menu de jeu  *)
let nomjeu = ref "";;
let numerogrille = ref "";;
(* Implémenter difficulte  *)
let numerodifficultejeu = ref "";;


let testkeyrange key =
  if(key ='1' || key ='2' ||key ='3' ||key ='4' ||key ='5' ||key ='6' ||key ='7' ||key ='8' || key ='9' || key ='a') then true else false;;

(* Fonction du menu principale permettant de lire au clavier le nom, le numéro de grille ainsi que la difficulté tout en l'affichant à l'écran *)
let reponseJ bool=
  if (bool = true) then (

    let ba = ref true in
    let nbNom = ref 10 in
    let nbGrille = ref 3 in
    let nbDifficulte = ref 2 in
    let u = ref 720 in
    let v = ref 720 in
    let w =  ref 720 in
    while !ba do
      let s = Graphics.wait_next_event [Graphics.Button_down]
      in match s.Graphics.mouse_y with
      |nom when (s.Graphics.mouse_y > 445 && s.Graphics.mouse_y < 495 && s.Graphics.mouse_x > 700 && s.Graphics.mouse_x < 900 )->
        begin
          let encore = ref true in
          while (!nbNom > 0 && !encore) do
            begin
              Graphics.moveto !u 460; 
              if Graphics.key_pressed () then (nbNom := (!nbNom - 1); let key = read_key () in ( Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0);Graphics.draw_char key ; u:=!u +15;
                                                                                                 nomjeu := !nomjeu ^ (String.make 1 (key)))) 

              else if ((Graphics.button_down ()) && ((snd (Graphics.mouse_pos ())) < 445 || (snd (Graphics.mouse_pos ())) > 495) && ((fst (Graphics.mouse_pos ())) > 700 || (fst (Graphics.mouse_pos ())) < 900)) then ( encore := false) else ()
            end
          done

        end


      |grille when (s.Graphics.mouse_y > 345 && s.Graphics.mouse_y < 395 && s.Graphics.mouse_x > 700 && s.Graphics.mouse_x < 800 )->
        begin
          let encore = ref true in
          while (!nbGrille > 0 && !encore) do
            begin


              Graphics.moveto !v 360;
              if Graphics.key_pressed () then ( let key = read_key () in ( if ((testkeyrange key) || key ='0') then ( nbGrille := (!nbGrille - 1);Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0);Graphics.draw_char key ;v:=!v +15;
                                                                                                                      numerogrille := !numerogrille ^ (String.make 1 (key))) else () ))

              else if ((Graphics.button_down ()) && ((snd (Graphics.mouse_pos ())) < 345 || (snd (Graphics.mouse_pos ())) > 395) && ((fst (Graphics.mouse_pos ())) > 700 || (fst (Graphics.mouse_pos ())) < 900)) then ( encore := false) else ()

            end
          done
        end


      |difficulte when (s.Graphics.mouse_y > 245 && s.Graphics.mouse_y < 295 && s.Graphics.mouse_x > 700 && s.Graphics.mouse_x < 800 )-> 
        begin
          let encore = ref true in
          while (!nbDifficulte > 0 && !encore) do
            begin
              Graphics.moveto !w 260;
              if Graphics.key_pressed () then ( let key = read_key () in (if ((testkeyrange key) || key ='0') then (nbDifficulte := (!nbDifficulte - 1);Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0);Graphics.draw_char key ; w:=!w +15;
                                                                                                                    numerodifficultejeu := !numerodifficultejeu ^ (String.make 1 (key)))))

              else if ((Graphics.button_down ()) && ((snd (Graphics.mouse_pos ())) < 245 || (snd (Graphics.mouse_pos ())) > 295) && ((fst (Graphics.mouse_pos ())) > 700 || (fst (Graphics.mouse_pos ())) < 900)) then ( encore := false) else ()

            end
          done
        end

      |valide when s.Graphics.mouse_y > 125 && s.Graphics.mouse_y < 175-> if s.Graphics.mouse_x > 540 && s.Graphics.mouse_x < 740 then begin Graphics.clear_graph(); ba:= false end
      |_ -> ();
    done;
  ) else ()
;;


(* Fonction referencemenu permettant de prendre des mesures si l'utilisateur ne rentre rien au clavier
   exemple : si l'utilisateur ne rentre pas de nom, lui associer Bob directement, si il ne spécifie pas de numéro de grille, alors en choisir une aléatoirement
   et si pas de numero de difficulté choisi, alors le prendre aléatoirement aussi
*)
let referencemenu bool = 
  if (bool = true) then
    begin
      (if (!nomjeu = "") then nomjeu := "Bob" else ());
      (if (!numerogrille = "" || (!numerogrille > string_of_int(243))) then ( let valrandom = (Random.int (243)) in (numerogrille := string_of_int(valrandom))) else ()) ;
      (if ((!numerodifficultejeu = "") || (!numerodifficultejeu > string_of_int(40))) then (let valrandom = (Random.int (40)) in (numerodifficultejeu := string_of_int(valrandom)  ) ) else ());
    end

(* then (let valrandom = (Random.int (40)) in  *)
(* Fonction permettant de charger l'image de début ainsi que de lancer la fonction de menu pour lier les entrées clavier *)
let lancermenudebut bool =
  if (bool = true) then (menuDebut true ; reponseJ true; referencemenu true) else ();;


(* On lance cette fonction *)
lancermenudebut true;;


(*  Fonction permettant de dessiner à l'écran le numéro que l'on attribue à la case de sudoku, uniquement au cas par cas  *)
let dessineNb case valeur =
  Graphics.set_text_size 50;
  Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
  Graphics.set_color (rgb 0 255 0);


  Graphics.moveto (120 + (case mod 9)*55 + ((case mod 9)/3)*3) (560 - (case/9)*55 - (case/27)*3);

  if ((int_of_char (valeur)-48) != 0) then begin
    Graphics.draw_string (String.make 1 valeur);
  end

  else
    begin
      (* Graphics.draw_string (String.make 1 '9') ; *)
    end
;;



(*  Fonction permettant de dessiner les chiffres dans la grille entièrement en prenant une grille entière en paramètre  *)
let dessineSudoku grille =
  for i = 0 to 80 do
    dessineNb i grille.[i];
  done;;

(* Fonction permettant de dessiner chaque case par région de sudoku, en tout il y a 9 cases par région  *)
let dessineRegion x y =
  Graphics.set_color black;
  for i = 0 to 8 do
    Graphics.fill_rect (x+(i mod 3)*55+2) (y+(i/3)*55+2) 51 51;
  done;;

(* Fonction permettant de dessiner chaque région de sudoku, il y a en tout 9 régions  *)
let dessineGrille bool=
  for i = 0 to 8 do
    dessineRegion (100+(i mod 3)*3+(i mod 3)*165) (100+(i/3)*3+(i/3)*165);
  done;;




(* Fonction prenant en paramètre une grillesudoku (un string) et en renvoyant un array associé à cette grille
   Fonction très utile pour la suite du programme
*)

let getarrayfromgrille grillesudokuB= let array = Array.make 81 0 in
  for k = 0 to 80 do
    array.(k) <-(int_of_char(String.get grillesudokuB (k))-48);
  done;
  array;;


(* Fonction prenant en paramètre une grille et une array et permet de renvoyer la grille avec comme valeur les valeurs de arraygrille
   Utile pour la suite du programme
*)

let getgrillefromarray arraygrille =
  let grille = ref "000000000000000000000000000000000000000000000000000000000000000000000000000000000" in
  for k = 0 to 80 do
    Bytes.set !grille k (char_of_int(arraygrille.(k)+48));
  done; !grille;;


(* Fonction permettant de renvoyer un array contenant toutes les valeurs des abscisses pour toutes les cases du sudoku
   Fonction indispensable pour getCaseCoord

*)


(* TODO optimiser mettre en doublet sft snd *)
let getarraycasex bool =
  let array = Array.make_matrix 9 9 0 in
  for k = 0 to 80 do
    array.(k/9).(k mod 9) <- (100 + (k mod 9)*55 +((k mod 9)/3)*3);
    (* Printf.printf"\n %d et %d | %d et %d\n "  (k/9) (k mod 9) (array.(k/9).(k mod 9).(0)) (array.(k/9).(k mod 9).(1));  *)
  done;
  array;;

(* Fonction permettant de renvoyer un array contenant toutes les valeurs des ordonnées pour toutes les cases du sudoku
   Fonction indispensable pour getCaseCoord
*)
let getarraycasey bool =
  let array = Array.make_matrix 9 9 0 in
  for k = 0 to 80 do
    array.(k/9).(k mod 9) <- (600 - (k/9)*55 - (k/27)*3);
    (* Printf.printf"\n %d et %d | %d et %d\n "  (k/9) (k mod 9) (array.(k/9).(k mod 9).(0)) (array.(k/9).(k mod 9).(1));  *)
  done;
  array;;


(*Fonction prenant en paramètre un int x et y et un arraycasex arraycase y et va permettre de renvoyer la valeur de la case dans laquelle
  l'utilisateur va cliquer. Fonction très importante pour le bon déroulement du jeu

*)
let getCaseCoord x y arraycasex arraycasey =
  let rec test k =
    if k >= 81 then -1 else
      begin
        if ((x > arraycasex.(k/9).(k mod 9) && x < (arraycasex.(k/9).(k mod 9)+55)) && (y< arraycasey.(k/9).(k mod 9) && y>(arraycasey.(k/9).(k mod 9)-55) )) then k
        else (test (k+1));
      end
  in test 0;;


(* Même fonction permettant de lire la grille solution associée à la grille que l'utilisateur a demandé *)
let grillesudokuSol = ref (Sudoku.lecturesolution (int_of_string(!numerogrille)));;


(* Même fonction permettant de lire la grille solution associée à la grille que l'utilisateur a demandé *)
let grillesudokuB = ref (Sudoku.lecturesolution (int_of_string(!numerogrille)));; 

(* Valeur grille temporaire permettant de get en forme d'array la grille !grillesudokuB*)
let grillesudokuBis = ref (getarrayfromgrille !grillesudokuB);;

(* On applique la fonction retireChiffres2 du fichier sudoku.ml permettant de retirer le nombre de chiffres qu'on veut
   Ici, plus on monte en difficulté, moins de chiffres seront supprimés
*)

(Sudoku.retireChiffres2 ( (20 + int_of_string(!numerodifficultejeu))) (!grillesudokuBis)) ;;
(* (Sudoku.retireChiffres2 ( (20 + int_of_string(!numerodifficultejeu))) (!grillesudokuBis)) ;; *)

let procedurealeatoire arraygrille arraysolution =
  let valrandomgrille = ref (Random.int (81)) in 
  let val1 = ref 0 in
  let bool = ref true in


  while (!bool) do
    if((arraygrille.(!valrandomgrille) = 0 ) && arraysolution.(!valrandomgrille) != 0)
    then ((arraygrille.(!valrandomgrille) <- arraysolution.(!valrandomgrille)); val1 := arraysolution.(!valrandomgrille);bool := false )
    else (valrandomgrille := ((!valrandomgrille + 1 ) mod 81 ))

  done; (!valrandomgrille)

(* Enfin, on récupère la grille associée plus haut avec la fonction getgrillefromarray *)
let grillesudokuB = ref (getgrillefromarray !grillesudokuBis);;



let getRealPos idCase = 
  ((100 + (idCase mod 9)*55 + ((idCase mod 9)/3)*3, 601 - (idCase / 9)*55 - (idCase/27)*3));;

let dessineSelect idCase efface =
  if (idCase < 0) then () else (
    if efface then Graphics.set_color white else Graphics.set_color cyan;
    let pos = (getRealPos idCase) in
    Graphics.fill_rect ((fst pos)-1) ((snd pos)-54) 2 55; (*Lisière gauche*)
    Graphics.fill_rect ((fst pos)-1) ((snd pos)-56) 55 2; (*Lisère basse*)
    Graphics.fill_rect ((fst pos)+54) ((snd pos)-56) 2 57; (*Lisière droite*)
    Graphics.fill_rect ((fst pos)-1) ((snd pos)-1) 55 2 (*Lisière hautes*)
  );;

(* Message qu'on affiche lorsqu'on perd la partie
   On n'oublie pas de remettre tous les paramètres de Graphics, à savoir la couleur, la size et la font
*)

let messageperdant bool =
  if bool = true then (Graphics.clear_graph();Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0); Graphics.set_text_size 100; Graphics.moveto 10 500;
                       Graphics.draw_string ("Vous avez perdu " ^ (!nomjeu) ^ ", :(. ");
                       Graphics.draw_string ("Souhaitez-vous reessayer " ^ (!nomjeu) ^"? ");
                       Graphics.draw_string ("tapez 'o' pour oui, 'n' pour retourner au menu principal et 'q' pour quitter, merci ! ");
                       Graphics.set_text_size 50; Graphics.set_color (rgb 0 255 0)) else ();;

(* Idem que la fonction d'avant mais pour message gagnant *)
let messagegagnant bool =
  if bool = true then (Graphics.clear_graph();Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0); Graphics.set_text_size 100; Graphics.moveto 200 500;
                       Graphics.draw_string ("Vous avez gagne " ^ (!nomjeu) ^ ", GG. Appuyez sur 'q' pour quitter ou 'n' pour retourner au menu principal"); Graphics.set_text_size 50;Graphics.set_color (rgb 0 255 0);Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1") else ();;

(* Idem que la fonction d'avant mais va permettre d'afficher le nombre de vies restantes et de l'actualiser à chaque coup *)
let messagevie bool =
  if bool = true then ( Graphics.set_color (rgb 0 0 0);Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";Graphics.set_text_size 100; Graphics.moveto 770 400;
                        Graphics.draw_string ("Il vous reste   " ^ (string_of_int(!Sudoku.vie)) ^ "   vies "); Graphics.set_text_size 50; Graphics.set_color (rgb 0 255 0);Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1") else ();;



(* Fonction permettant de dessiner un cercle sur messagevie pour le supprimer entièrement et le réafficher à chaque
   fois que le joueur pose une pièce. *)
let clearmenuscore bool =
  if bool = true then (  Graphics.set_color (rgb 255 255 255 );Graphics.fill_rect 750 395 400 30; Graphics.set_color (rgb 0 255 0)) else ();;


let clearmenuregle bool =
  if bool = true then (  Graphics.set_color (rgb 255 255 255 );Graphics.fill_rect 605 15 670 380; Graphics.set_color (rgb 0 255 0)) else ();;


(* Fonction permettant d'afficher le message principal du jeu, à savoir qu'on peut appuyer sur E pour effacer la dernière lettre ajoutée *)
let message bool =
  if bool = true then (Graphics.set_font "-*-fixed-medium-r-*--18-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0);Graphics.set_text_size 100; Graphics.moveto 620 600;
                       Graphics.draw_string "Appuyez sur Z pour faire apparaitre le menu des commandes et des regles" ;Graphics.moveto 620 530;Graphics.set_font "-*-fixed-medium-r-*--21-*-*-*-*-*-iso8859-1";
                       Graphics.draw_string("Vous avez choisi la grille numero " ^ (!numerogrille)  ^ " avec une difficulte de " ^ (!numerodifficultejeu));) else ();;

(* 
let messagecompteur bool = 
  if bool = true then (Graphics.set_color (rgb 0 0 0 );Graphics.set_font "-*-fixed-medium-r-*--16-*-*-*-*-*-iso8859-1";Graphics.moveto 650 500;Graphics.draw_string"dzqdqzdzqdzqdzqdzqdzqdzqdzqdzqdzq") else ();;
 *)


(* Programme prenant en paramètre une grille sudoku et permettant de dessiner la grille,
   dessiner les valeurs des cases et d'initialiser le message et le messagevie pour le démarrage d'une partie *)
let lancerprog grillesudoku =

  dessineGrille true;
  dessineSudoku grillesudoku ;
  message true;
  messagevie true;;


(* Fonction principale du fonctionnement du sudoku où plusieurs références ont été créees pour garder le type unit
   Le principe est de récupérer à l'aide de la fonction getCaseCoord la valeur de la case sur laquelle on clique pour ensuite actualiser l'array du jeu avec la nouvelle valeur
   tempretourarriere et tempgrilleretourarriere vont permettre de pouvoir utiliser l'option de retour en arrière dans le jeu
*)


let drawregles bool = Graphics.set_font "-*-fixed-medium-r-*--16-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 255 0 0);Graphics.moveto 630 350;
  Graphics.draw_string "Une grille de sudoku est divisee en 9 lignes, 9 colonnes et 9 carres.";Graphics.moveto 630 320;
  Graphics.draw_string "  - La ligne est un ensemble de neuf cases disposees horizontalement.";Graphics.moveto 630 300;
  Graphics.draw_string "  - La colonne est un ensemble de neuf cases disposees verticalement.";Graphics.moveto 630 280;
  Graphics.draw_string "  - Le carre est un ensemble de neuf cases disposees en carre de 3x3 cases.";Graphics.moveto 630 260;
  Graphics.draw_string "  - La grille etant composees de neuf de ces carres.";Graphics.moveto 630 240;
  Graphics.draw_string "Chacune de ces zones doit contenir tous les chiffres de 1 e 9.";Graphics.moveto 630 210;
  Graphics.draw_string "Chaque case recoit un chiffre de 1 e 9 et fait partie des trois zones.";Graphics.moveto 630 160;
  Graphics.draw_string "- Appuyez sur E pour revenir en arriere en effacant la derniere lettre ajoutee";Graphics.moveto 630 140;
  Graphics.draw_string "- Une fois la partie finie, appuyez sur O (lettre) pour recommencer la grille ou";Graphics.moveto 630 110;
  Graphics.draw_string "  sur N pour retourner au menu principal et enfin Q pour quitter le programme";Graphics.moveto 630 100;
  Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";;





let jouerSudoku bool=

  let var = ref 0 in
  let bool = ref true in
  let arraygrilleJ = ref (getarrayfromgrille !grillesudokuB) in
  let arraygrilleSol = getarrayfromgrille !grillesudokuSol in
  let arraycasex = getarraycasex true in
  let arraycasey = getarraycasey true in
  let staticarraygrille = (Array.copy !arraygrilleJ) in
  let tempretourarriere = ref (Array.copy !arraygrilleJ) in
  let tempgrilleretourarriere = ref (Sudoku.lecturegrid (int_of_string(!numerogrille))) in
  let boolretourarriere = ref true in

  (* Faire changement focus case en un temps *)
  while !bool do


    (* let reponseJ = (Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed]) in *)
    let pos = ref (0,0) in 
    (* let keyboard = ref (reponseJ.Graphics.keypressed) in *)
    let encore = ref true in 
    let boolregles = ref true in
    let val2 = ref 0 in


    while (!encore ) do 

      begin 
        if Graphics.key_pressed () then (
          let key = read_key () in if (testkeyrange key) then
            (var := getCaseCoord (fst !pos) (snd !pos) arraycasex arraycasey;encore := false;	dessineSelect !var true;	

             if !var = -1 then () else if((staticarraygrille.(!var) = 0) && !arraygrilleJ.(!var) != 0 ) then () else if (staticarraygrille.(!var)!= 0) then () else ( if (arraygrilleSol.(!var) = (int_of_char(key)-48) )
                                                                                                                                                                      then ( tempretourarriere := Array.copy !arraygrilleJ; !arraygrilleJ.(!var) <- (int_of_char(key)-48);(dessineNb (!var) key);boolretourarriere := true)

                                                                                                                                                                      else (  (Sudoku.vie := !Sudoku.vie -1);clearmenuscore true;messagevie true; if !Sudoku.vie = 0 then (bool := false ;Sudoku.vie := 10; messageperdant true)));
             if(key ='a') then ((val2 := (procedurealeatoire !arraygrilleJ arraygrilleSol) ); (dessineNb (!val2) (char_of_int(!arraygrilleJ.(!val2)+48));boolretourarriere := false);
                                (  (Sudoku.vie := !Sudoku.vie -1);clearmenuscore true;messagevie true; if !Sudoku.vie = 0 then (bool := false ;Sudoku.vie := 10; messageperdant true)) ) else ();
             if (!arraygrilleJ = arraygrilleSol) then (bool := false ;Sudoku.vie := 10; messagegagnant true ) else () )

          else

            (if (key ='e' && !boolretourarriere = true) then ( if ((!arraygrilleJ = staticarraygrille) || (!arraygrilleJ = !tempretourarriere)) then () else ((arraygrilleJ := Array.copy !tempretourarriere;tempgrilleretourarriere := (getgrillefromarray !arraygrilleJ) ;Graphics.clear_graph();lancerprog !tempgrilleretourarriere)))  else (if (key ='z' && !boolregles = true) then (drawregles bool;boolregles := false) else ( if (key ='z' && !boolregles = false) then (clearmenuregle true;boolregles := true) else ())));

        ) else if(Graphics.button_down ()) then ( dessineSelect (getCaseCoord (fst !pos) (snd !pos) arraycasex arraycasey) true; pos := (Graphics.mouse_pos ()); dessineSelect (getCaseCoord (fst !pos) (snd !pos) arraycasex arraycasey) false 
                                                ) else ()
      end
    done
  done ;;



(* On relance le tout une fois qu'on a fini en spécifiant une nouvelle grille de sudoku par l'utilisateur
   et en redessinant toute la grille avec toutes les valeurs correspondantes *)
let relancerletout bool =
  if (bool = true) then (
    numerogrille := "";

    lancermenudebut true;
    grillesudokuB := (Sudoku.lecturegrid (int_of_string(!numerogrille)));
    grillesudokuSol := (Sudoku.lecturesolution (int_of_string(!numerogrille)));
    dessineGrille true;
    dessineSudoku !grillesudokuB ;
    message true;
    messagevie true;
    jouerSudoku true;

  ) else ();;


(* On lance une fois le programme avec la fonction principale jouerSudoku *)
lancerprog !grillesudokuB;;
jouerSudoku true;;

(* Fonction permettant de pouvoir recommencer la grille en cas de défaite *)
let testbool = ref true in
while (!testbool) do
  let key = read_key () in (if (key = 'o') then (Graphics.clear_graph();lancerprog !grillesudokuB;jouerSudoku true) else if (key ='n') then (testbool := false;Graphics.clear_graph()) else (if (key ='q' ) then (exit 0) else ()) );

done;;


(* Boucle while du programme où on relance entièrement le programme si l'utilisateur l'a choisi
   Dans ce cas, ne pas oublier de reboucler sur la reponse pour pouvoir recommencer la même grille en cas de défaite
*)
while (true) do

  relancerletout true ;
  (
    let testbool = ref true in
    while (!testbool) do
      let key = read_key () in (if (key = 'o') then (Graphics.clear_graph();lancerprog !grillesudokuB;jouerSudoku true) else if (key ='n') then (testbool := false;Graphics.clear_graph()) else ( if (key ='q' ) then (exit 0) else ()) );
    done;)
done;;

(* jouerSudoku true;;  *)

(* dessineSudoku grillesudokuB;; *)


(* Printf.printf "%d" Graphics.foreground; *)

(* let dessine2 =
   Sudoku.test2 10 10;; *)