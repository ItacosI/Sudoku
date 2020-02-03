open Graphics;;
let height = 700;;
open_graph " 700x700";;


let dessineRegion x y =
	Graphics.set_color black;
	for i = 0 to 8 do
		Graphics.fill_rect (x+(i mod 3)*55) (y+(i/3)*55) 53 53;
	done;;

let dessineGrille = 
		for i = 0 to 8 do
			dessineRegion (100+(i mod 3)*3+(i mod 3)*165) (100+(i/3)*3+(i/3)*165);
		done;;
		
let dessineMenu =
	Graphics.fill_arc 350 700 350 50 0 (-180);
	Graphics.fill_arc 350 0 350 50 0 180;;

let dessineNb case =
	Graphics.set_font "-bitstream-*-*-*-*--55-*-*-*-*-*-iso8859-1";
	(* "-*-fixed-medium-r-*--55-*-*-*-*-*-iso8859-1" *)
	Graphics.set_color (rgb 0 255 0);
	Printf.printf "x: %d  y: %d" (100+ 30 + (case mod 9)*55 + ((case mod 9)/3)*3) (546 - 15 - (case/9)*55 - (case/27)*3);
	Graphics.moveto (100+ 12 + (case mod 9)*55 + ((case mod 9)/3)*3) (546 - 5 - (case/9)*55 - (case/27)*3);
	Graphics.draw_string "2";;


dessineGrille;;
dessineMenu;;
dessineNb 0;;

Printf.printf "%d" Graphics.foreground;

while true do
	1;
done;;
