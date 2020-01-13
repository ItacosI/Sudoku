

let matr = [|[|1;2;3;4;5;6;7;8;9|];
            [|10;11;12;13;14;15;16;17;18|]|];

exception Faux;

(*Test si le nombre placé n'est pas deja present dans la colonne *)
let testCol ma i j va =
  let rec test1 k =
  if k >= 9 then true else
  begin
    if ma.(k).(j) == va  &&  not (k == i) then raise Faux else test1 (k + 1)
    end
      in try test1 0 with Faux -> false



(*Test si le nombre place n'est pas deja present dans la ligne *)

(*let testLig ma i j va =
  let rec test1 k acc =
    if k = j then test1 (k + 1) acc else
      begin if ma.(i).(k) = va then raise Faux else test1 (k + 1) acc end
      in try test1 0 true with Faux -> false*)


let testLig ma i j va =
  let rec test1 k =
    if k >= 9 then true else
      begin
        if ma.(i).(k) == va  &&  not (k == j) then raise Faux else test1 (k + 1)
      end
      in try test1 0 with Faux -> false




  (*recherche la zone 0 ou 1 ou 2 *)
  let rechercheCadre k =
  if k <= 3 then 0 else begin if k >3 && k <= 6 then 1 else 2  end


(*Test si le nombre place n'est pas deja present dans la case*)

    let cadre ma i j va =
    try
    let posi = rechercheCadre i in let posj = rechercheCadre j in
      for x = posi to (posi + 3) do
        for  y = posj to (posj + 3) do
          if x = i && y = j then () else begin
            if ma.(i).(j) = va then raise Faux else () end
        done
      done;
      true
      with Faux -> false


(*teste si un valeur est bonne dans une case *)
  let test ma i j va = cadre ma i j va && testLig ma i j va && testCol ma i j va
  
  
  (*Test la grille finale *)
  let testFinal ma =
    let res = ref true in
    for i = 0 to 9 do
      for j = 0 to 9 do
        res := (!res && test ma i j ma.(i).(j))
      done
    done ; !res
  

  let transfo s =
    let table = Array.make_matrix 9 9 0 in
    for k = 0 to 80 do
      table.(k/9).(k mod 9) <- int_of_char (s.[k]);
    done;
    table;;
