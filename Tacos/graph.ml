open Graphics;;
open Sudoku;;
let height = 700;;


open_graph " 1280x720";;


(* let dessineMenu =
	Graphics.fill_arc 640 700 350 50 0 (-180);
	Graphics.fill_arc 640 0 350 50 0 180;; *)




let menuDebut =
	(* Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
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
	Graphics.draw_string "ENTREE";;
	 *)
	draw_image (Image.init_image "yol.ppm") 0 0;;

let reponseJ =
	let b = ref true in
	while !b do
	 let s = Graphics.wait_next_event [Graphics.Button_down]
		in match s.Graphics.mouse_y with
			|a when s.Graphics.mouse_y > 225 && s.Graphics.mouse_y < 275 -> if s.Graphics.mouse_x > 700 && s.Graphics.mouse_x < 900  then begin Graphics.moveto 800 260; let key = read_key () in Graphics.draw_char key end
			|b when s.Graphics.mouse_y > 325 && s.Graphics.mouse_y < 375 -> if s.Graphics.mouse_x > 700 && s.Graphics.mouse_x < 800  then begin Graphics.moveto 750 350; let key = read_key () in Graphics.draw_char key end
			|c when s.Graphics.mouse_y > 425 && s.Graphics.mouse_y < 475 -> if s.Graphics.mouse_x > 700 && s.Graphics.mouse_x < 800 then begin Graphics.moveto 750 450; let key = read_key () in Graphics.draw_char key end
			|d when s.Graphics.mouse_y > 445 && s.Graphics.mouse_y < 495 -> if s.Graphics.mouse_x > 540 && s.Graphics.mouse_x < 740 then begin Graphics.clear_graph(); b:= false end
			|_ -> ();
		done
	;;


let dessineRegion x y =
	Graphics.set_color black;
	for i = 0 to 8 do
		Graphics.fill_rect (x+(i mod 3)*55) (y+(i/3)*55) 53 53;
	done;;

let dessineGrille =
	for i = 0 to 8 do
		dessineRegion (100+(i mod 3)*3+(i mod 3)*165) (100+(i/3)*3+(i/3)*165);
	done;;


let dessineNb case valeur =
	Graphics.set_text_size 50;
	Graphics.set_font "-*-fixed-medium-r-*--24-*-*-*-*-*-iso8859-1";
	Graphics.set_color (rgb 0 255 0);

	Printf.printf "x: %d  y: %d" (100 + (case mod 9)*55 + ((case mod 9)/3)*3) (546 - (case/9)*55 - (case/27)*3);
	Graphics.moveto (120 + (case mod 9)*55 + ((case mod 9)/3)*3) (560 - (case/9)*55 - (case/27)*3);

	if ((int_of_char (valeur)-48) != 0) then 
		begin
			Graphics.draw_string (String.make 1 valeur) ;
		end
	else
		begin
			 (* Graphics.draw_string (String.make 1 '9') ; *)
		 end
	;;

let grillesudokuB = "807000003602080000000200900040005001000798000200100070004003000000040108300000506";;



let dessineSudoku grille =
	for i = 0 to 80 do
		dessineNb i grille.[i];
	done;;



dessineGrille;;
(* dessineMenu;; *)
dessineSudoku grillesudokuB;;

Printf.printf "%d" Graphics.foreground;

while true do
	1;
done;;
