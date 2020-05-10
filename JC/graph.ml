
open Graphics;;
let height = 700;;


(* On open la partie graphique en spécifiant la résolution *)
open_graph " 1280x720";;

(*
let dessineMenu =
Graphics.fill_arc 640 700 350 50 0 (-180);
Graphics.fill_arc 640 0 350 50 0 180;;
*)



(* Fonction permettant de load l'immage yol.ppm *)
let menuDebut bool=

(*
Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
Graphics.moveto 580 620;
Graphics.draw_string "BIENVENUE";
Graphics.moveto 450 500;
Graphics.draw_string "Quelle grille voulez-vous (0 - 243) :";
Graphics.moveto 900 500;
Graphics.moveto 450 400;
Graphics.draw_string "Difficulte (0 - 40) : ";
Graphics.moveto 707 400;
Graphics.moveto 450 200;
fill_rect 540 202 20 20;
Graphics.draw_string "ENTREE"
;;*)
  if(bool = true) then
    (draw_image(Image.init_image "yol.ppm") 0 0) else ();;


(* Variables réference du menu de jeu  *)
let nomjeu = ref "";;
let numerogrille = ref "";;
let numerodifficultejeu = ref "";;


(* Fonction permettant de tester si les entrées au clavier sont bien des chiffres, pas de 0 pour pas positionner de 0 dans la grille *)
let testkeyrange key =
  if(key ='1' || key ='2' ||key ='3' ||key ='4' ||key ='5' ||key ='6' ||key ='7' ||key ='8' || key ='9') then true else false;;

(* Fonction du menu principale permettant de lire au clavier le nom, le numéro de grille ainsi que la difficulté tout en l'affichant à l'écran *)
let reponseJ bool=
  if (bool = true) then (

    let ba = ref true in
    let nbNom = ref 10 in
    let nbGrille = ref 3 in
    let nbDiffuculte = ref 2 in
    let u = ref 720 in
    let v = ref 720 in
    let w =  ref 720 in 
    while !ba do
      let s = Graphics.wait_next_event [Graphics.Button_down]
      in match s.Graphics.mouse_y with
      |nom when s.Graphics.mouse_y > 445 && s.Graphics.mouse_y < 495 -> if s.Graphics.mouse_x > (!u - 50) && s.Graphics.mouse_x < (!u + 50)  then
          begin
            while !nbNom > 0 do
              begin
                Graphics.moveto !u 460; nbNom := (!nbNom - 1) ;
                let key = read_key () in if int_of_char(key) = 13 then nbNom := 0 else ( Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0);Graphics.draw_char key ; u:=!u +15; 
                                                                                         nomjeu := !nomjeu ^ (String.make 1 (key)))
              end
            done
          end


      |grille when s.Graphics.mouse_y > 345 && s.Graphics.mouse_y < 395 -> if s.Graphics.mouse_x > (!v - 50) && s.Graphics.mouse_x < (!v + 50)  then
          begin
            while !nbGrille > 0 do
              begin

                let key = read_key () in if int_of_char(key) = 13 then nbGrille := 0 else ( if ((testkeyrange key) || key ='0') then (Graphics.moveto !v 360; nbGrille := (!nbGrille - 1);Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0);Graphics.draw_char key ;v:=!v +15;
                                                                                                                                      numerogrille := !numerogrille ^ (String.make 1 (key))) else ())


              end
            done
          end


      |difficulte when s.Graphics.mouse_y > 245 && s.Graphics.mouse_y < 295 -> if s.Graphics.mouse_x > (!w - 50)&& s.Graphics.mouse_x <(!w + 50) then
          begin
            while !nbDiffuculte > 0 do
              begin

                let key = read_key () in if int_of_char(key) = 13 then nbDiffuculte := 0 else (if ((testkeyrange key) || key ='0')then  (Graphics.moveto !w 260;nbDiffuculte := (!nbDiffuculte - 1);Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0);Graphics.draw_char key ; w:=!w +15;
                                                                                                                                         numerodifficultejeu := !numerodifficultejeu ^ (String.make 1 (key))))
              end
            done
          end

      |valide when s.Graphics.mouse_y > 125 && s.Graphics.mouse_y < 175-> if s.Graphics.mouse_x > 540 && s.Graphics.mouse_x < 740 then begin Graphics.clear_graph(); ba:= false end
      |_ -> ();
    done;
  ) else ()
;;


(* Fonction permettant de charger l'image de début ainsi que de lancer la fonction de menu pour lier les entrées clavier *)
let lancermenudebut bool = 
  if (bool = true) then (menuDebut true ; reponseJ true) else ();;


(* On lance cette fonction *)
lancermenudebut true;;


(*  Fonction permettant de dessiner à l'écran le numéro que l'on attribue à la case de sudoku, uniquement au cas par cas  *)
let dessineNb case valeur =
  Graphics.set_text_size 50;
  Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
  Graphics.set_color (rgb 0 255 0);

  Printf.printf "x: %d  y: %d " (100 + (case mod 9)*55 + ((case mod 9)/3)*3) (546 - (case/9)*55 - (case/27)*3);

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
    Graphics.fill_rect (x+(i mod 3)*55) (y+(i/3)*55) 53 53;
  done;;

(* Fonction permettant de dessiner chaque région de sudoku, il y a en tout 9 régions  *)
let dessineGrille bool= 
  for i = 0 to 8 do
    dessineRegion (100+(i mod 3)*3+(i mod 3)*165) (100+(i/3)*3+(i/3)*165);
  done;;



(* Fonction permettant de lire la grille que l'utilisateur a demandé à l'aide de !numerogrille
   C'est une fonction déjà implémentée dans sudoku.ml, on l'appelle donc de cette manière
*)
let grillesudokuB = ref (Sudoku.lecturegrid (int_of_string(!numerogrille)));;



(* Même fonction permettant de lire la grille solution associée à la grille que l'utilisateur a demandé *)
let grillesudokuSol = ref (Sudoku.lecturesolution (int_of_string(!numerogrille)));;


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

let getgrillefromarray grille arraygrille = 
  for k = 0 to 80 do
    Bytes.set grille k (char_of_int(arraygrille.(k)+48));
  done;;


(* Fonction permettant de renvoyer un array contenant toutes les valeurs des abscisses pour toutes les cases du sudoku
   Fonction indispensable pour getCaseCoord

*)
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
                       Graphics.draw_string ("Vous avez gagne " ^ (!nomjeu) ^ ", GG. Appuyez sur 'q' pour quitter "); Graphics.set_text_size 50;Graphics.set_color (rgb 0 255 0);Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1") else ();;

(* Idem que la fonction d'avant mais va permettre d'afficher le nombre de vies restantes et de l'actualiser à chaque coup *)
let messagevie bool =
  if bool = true then ( Graphics.set_color (rgb 0 0 0);Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";Graphics.set_text_size 100; Graphics.moveto 800 300;
                        Graphics.draw_string ("Il vous reste   " ^ (string_of_int(!Sudoku.vie)) ^ "   vies "); Graphics.set_text_size 50; Graphics.set_color (rgb 0 255 0);Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1") else ();;


(* Fonction permettant de dessiner un cercle sur messagevie pour le supprimer entièrement et le réafficher à chaque 
   fois que le joueur pose une pièce. *)
let clearmenuscore bool = 
  if bool = true then (  Graphics.set_color (rgb 255 255 255);Graphics.draw_circle 950 250 250;Graphics.fill_circle 950 250 250; Graphics.set_color (rgb 0 255 0)) else ();;


(* Fonction permettant d'afficher le message principal du jeu, à savoir qu'on peut appuyer sur E pour effacer la dernière lettre ajoutée *)
let message bool =
  if bool = true then (Graphics.set_font "-*-fixed-medium-r-*--15-*-*-*-*-*-iso8859-1";Graphics.set_color (rgb 0 0 0);Graphics.set_text_size 100; Graphics.moveto 650 600;
                       Graphics.draw_string "Appuyez sur la touche E pour effacer la derniere lettre ajoutee" ;Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1") else ();;


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


  while !bool do 
    let reponseJ = Graphics.wait_next_event [Graphics.Button_down] in
    let key = read_key () in if (testkeyrange key) then
      (var := getCaseCoord reponseJ.Graphics.mouse_x reponseJ.Graphics.mouse_y arraycasex arraycasey; messagevie true;
       if !var = -1 then () else if (staticarraygrille.(!var)!= 0) then () else ( if (arraygrilleSol.(!var) = (int_of_char(key)-48) )
                                                                                  then ( tempretourarriere := Array.copy !arraygrilleJ; !arraygrilleJ.(!var) <- (int_of_char(key)-48);(dessineNb (!var) key))
                                                                                  else (  (Sudoku.vie := !Sudoku.vie -1) ;clearmenuscore true;messagevie true; if !Sudoku.vie = 0 then (bool := false ;Sudoku.vie := 10; messageperdant true)));
       if (!arraygrilleJ = arraygrilleSol) then (bool := false ;Sudoku.vie := 10; messagegagnant true ) else () )
    else



      (if (key ='e') then ( if (staticarraygrille = !arraygrilleJ) then () else ((arraygrilleJ := Array.copy !tempretourarriere;getgrillefromarray !tempgrilleretourarriere !arraygrilleJ ;Graphics.clear_graph();lancerprog !tempgrilleretourarriere)))  else ());
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
  let reponseE = Graphics.wait_next_event [Graphics.Button_down] in let key = read_key () in (if (key = 'o') then (Graphics.clear_graph();lancerprog !grillesudokuB;jouerSudoku true) else if (key ='n') then (testbool := false;Graphics.clear_graph()) else (if (key ='q' ) then (exit 0) else ()) );

done;;


(* Boucle while du programme où on relance entièrement le programme si l'utilisateur l'a choisi
   Dans ce cas, ne pas oublier de reboucler sur la reponse pour pouvoir recommencer la même grille en cas de défaite
*)
while (true) do 

  relancerletout true ;
  ( 
    let testbool = ref true in
    while (!testbool) do
      let reponseE = Graphics.wait_next_event [Graphics.Button_down] in let key = read_key () in (if (key = 'o') then (Graphics.clear_graph();lancerprog !grillesudokuB;jouerSudoku true) else if (key ='n') then (testbool := false;Graphics.clear_graph()) else ( if (key ='q' ) then (exit 0) else ()) );
    done;)
done;; 

(* jouerSudoku true;;  *)

(* dessineSudoku grillesudokuB;; *)


(* Printf.printf "%d" Graphics.foreground; *)

(* let dessine2 = 
   Sudoku.test2 10 10;; *)

