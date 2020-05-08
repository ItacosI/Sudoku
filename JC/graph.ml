
open Graphics;;
let height = 700;;



open_graph " 1280x720";;

(*
let dessineMenu =
Graphics.fill_arc 640 700 350 50 0 (-180);
Graphics.fill_arc 640 0 350 50 0 180;;
*)


let menuDebut =

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
  draw_image(Image.init_image "yol.ppm") 0 0;;


let nomjeu = ref "";;
let numerogrille = ref "";;
let numerodifficultejeu = ref "";;


let reponseJ =
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
              let key = read_key () in if int_of_char(key) = 13 then nbNom := 0 else (Graphics.draw_char key ; u:=!u +15; 
                                                                                      nomjeu := !nomjeu ^ (String.make 1 (key)))
            end
          done
        end


    |grille when s.Graphics.mouse_y > 345 && s.Graphics.mouse_y < 395 -> if s.Graphics.mouse_x > (!v - 50) && s.Graphics.mouse_x < (!v + 50)  then
        begin
          while !nbGrille > 0 do
            begin
              Graphics.moveto !v 360; nbGrille := (!nbGrille - 1);
              let key = read_key () in if int_of_char(key) = 13 then nbGrille := 0 else (Graphics.draw_char key ;v:=!v +15;
                                                                                         numerogrille := !numerogrille ^ (String.make 1 (key)))


            end
          done
        end


    |difficulte when s.Graphics.mouse_y > 245 && s.Graphics.mouse_y < 295 -> if s.Graphics.mouse_x > (!w - 50)&& s.Graphics.mouse_x <(!w + 50) then
        begin
          while !nbDiffuculte > 0 do
            begin
              Graphics.moveto !w 260;nbDiffuculte := (!nbDiffuculte - 1);
              let key = read_key () in if int_of_char(key) = 13 then nbDiffuculte := 0 else (Graphics.draw_char key ; w:=!w +15;
                                                                                             numerodifficultejeu := !numerodifficultejeu ^ (String.make 1 (key)))
            end
          done
        end

    |valide when s.Graphics.mouse_y > 125 && s.Graphics.mouse_y < 175-> if s.Graphics.mouse_x > 540 && s.Graphics.mouse_x < 740 then begin Graphics.clear_graph(); ba:= false end
    |_ -> ();
  done
;;



Printf.printf"%s\n" !nomjeu;;
Printf.printf"%s\n" !numerogrille;;
Printf.printf"%s\n" !numerodifficultejeu;;



let dessineNb case valeur =
  Graphics.set_text_size 50;
  Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
  Graphics.set_color (rgb 0 255 0);

  Printf.printf "x: %d  y: %d " (100 + (case mod 9)*55 + ((case mod 9)/3)*3) (546 - (case/9)*55 - (case/27)*3);

  Graphics.moveto (120 + (case mod 9)*55 + ((case mod 9)/3)*3) (560 - (case/9)*55 - (case/27)*3);

  if ((int_of_char (valeur)-48) != 0) then begin
    Graphics.draw_string (String.make 1 valeur) ;
  end
  else
    begin
      (* Graphics.draw_string (String.make 1 '9') ; *)
    end
;;

let dessineSudoku grille =
  for i = 0 to 80 do
    dessineNb i grille.[i];
  done;;


let dessineRegion x y =
  Graphics.set_color black;
  for i = 0 to 8 do
    Graphics.fill_rect (x+(i mod 3)*55) (y+(i/3)*55) 53 53;
  done;;

let dessineGrille bool= 
  for i = 0 to 8 do
    dessineRegion (100+(i mod 3)*3+(i mod 3)*165) (100+(i/3)*3+(i/3)*165);
  done;;




let grillesudokuB = (Sudoku.lecturegrid (int_of_string(!numerogrille)));;

let grillesudokuSol = (Sudoku.lecturesolution (int_of_string(!numerogrille)));;



let getarraygrilleJ grillesudokuB= let array = Array.make 81 0 in
  for k = 0 to 80 do 
    array.(k) <-(int_of_char(String.get grillesudokuB (k))-48); 
  done;
  array;;

let getarraygrilleSol grillesudokuSol = let array = Array.make 81 0 in
  for k = 0 to 80 do 
    array.(k) <- (int_of_char(String.get grillesudokuSol (k))-48); 
  done; 
  array;;

(* Printf.printf"\n\n\n%d\n\n\n" arraygrilleSol.(1);; *)


let getarraycasex bool = 
  let array = Array.make_matrix 9 9 0 in
  for k = 0 to 80 do 
    array.(k/9).(k mod 9) <- (100 + (k mod 9)*55 +((k mod 9)/3)*3);
    (* Printf.printf"\n %d et %d | %d et %d\n "  (k/9) (k mod 9) (array.(k/9).(k mod 9).(0)) (array.(k/9).(k mod 9).(1));  *)
  done;
  array;;


let getarraycasey bool = 
  let array = Array.make_matrix 9 9 0 in
  for k = 0 to 80 do 
    array.(k/9).(k mod 9) <- (600 - (k/9)*55 - (k/27)*3);
    (* Printf.printf"\n %d et %d | %d et %d\n "  (k/9) (k mod 9) (array.(k/9).(k mod 9).(0)) (array.(k/9).(k mod 9).(1));  *)
  done;
  array;;

let getCaseCoord x y arraycasex arraycasey =
  let rec test k = 
    if k >= 81 then -1 else 
      begin 
        if ((x > arraycasex.(k/9).(k mod 9) && x < (arraycasex.(k/9).(k mod 9)+55)) && (y< arraycasey.(k/9).(k mod 9) && y>(arraycasey.(k/9).(k mod 9)-55) )) then k
        else (test (k+1));
      end
  in test 0;;


let messageperdant bool =
  if bool = true then (Graphics.clear_graph(); Graphics.set_text_size 100; Graphics.moveto 200 500;
                       Graphics.draw_string ("Vous avez perdu " ^ (!nomjeu) ^ ", :(. ");
                       Graphics.draw_string ("Souhaitez-vous reessayer " ^ (!nomjeu) ^"? ");
                       Graphics.draw_string ("tapez o pour oui et  pour non, merci ! ");
                       Graphics.set_text_size 50) else ();;

let messagegagnant bool =
  if bool = true then (Graphics.clear_graph(); Graphics.set_text_size 100; Graphics.moveto 200 500;
                       Graphics.draw_string ("Vous avez gagné " ^ (!nomjeu) ^ ", GG. "); Graphics.set_text_size 50) else ();;


let messagevie bool =
  if bool = true then (Graphics.set_color (rgb 0 0 0);Graphics.set_text_size 100; Graphics.moveto 800 300;
                       Graphics.draw_string ("Il vous reste   " ^ (string_of_int(!Sudoku.vie)) ^ "   vies "); Graphics.set_text_size 50; Graphics.set_color (rgb 0 255 0)) else ();;


let clearmenuscore bool = 
  if bool = true then (  Graphics.set_color (rgb 255 255 255);Graphics.draw_circle 950 250 250;Graphics.fill_circle 950 250 250; Graphics.set_color (rgb 0 255 0)) else ();;



dessineGrille true;;
dessineSudoku grillesudokuB ;;
messagevie true;;

let jouerSudoku bool=

  let var = ref 0 in
  let bool = ref true in
  let arraygrilleJ = getarraygrilleJ grillesudokuB in
  let arraygrilleSol = getarraygrilleSol grillesudokuSol in
  let arraycasex = getarraycasex true in
  let arraycasey = getarraycasey true in
  let temp = (Array.copy arraygrilleJ) in



  while !bool do 
    let reponseJ = Graphics.wait_next_event [Graphics.Button_down] in
    let key = read_key () in 
    (var := getCaseCoord reponseJ.Graphics.mouse_x reponseJ.Graphics.mouse_y arraycasex arraycasey;messagevie true;
     if !var = -1 then () else if (temp.(!var)!= 0) then () else ( if (arraygrilleSol.(!var) = (int_of_char(key)-48) )  then ( arraygrilleJ.(!var) <- (int_of_char(key)-48);(dessineNb (!var) key)) else (  (Sudoku.vie := !Sudoku.vie -1) ;clearmenuscore true;messagevie true; if !Sudoku.vie = 0 then (bool := false ;Sudoku.vie := 10; messageperdant true)));
     if (arraygrilleJ = arraygrilleSol) then (bool := false ;Sudoku.vie := 10; messagegagnant true) else () );

  done ;;

jouerSudoku true;;

let testbool = ref true in 
while (!testbool) do
  let reponseE = Graphics.wait_next_event [Graphics.Button_down] in let key = read_key () in (if (key = 'o') then (Graphics.clear_graph();dessineGrille true;dessineSudoku grillesudokuB ;messagevie true;jouerSudoku true) else if (key ='n') then (testbool := false;Graphics.clear_graph()) else () );

done;;

(* jouerSudoku true;;  *)

(* dessineSudoku grillesudokuB;; *)


(* Printf.printf "%d" Graphics.foreground; *)

(* let dessine2 = 
   Sudoku.test2 10 10;; *)



while true do
  1;
done;;
