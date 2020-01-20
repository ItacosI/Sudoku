 exception Faux;;

(*Test si le nombre placé n'est pas deja present dans la colonne 
ma -> matrice a tester 
i j -> coordonnee de la case a tester
va -> nombre a tester*)

let testCol ma i j va =
  let rec test1 k =
  if k >= 9 then true else
  begin
    if ma.(k).(j) == va then raise Faux else test1 (k + 1)
    end
      in try test1 0 with Faux -> false




(*Test si le nombre place n'est pas deja present dans la ligne 
ma -> matrice a tester 
i j -> coordonnee de la case a tester
va -> nombre a tester*)

let testLig ma i j va =
  let rec test1 k =
    if k >= 9 then true else
      begin
        if ma.(i).(k) == va then raise Faux else test1 (k + 1)
      end
      in try test1 0 with Faux -> false





(*recherche de la coordonnée de la premiere case du bloc dans laquelle est la case a tester *)

  let rechercheCadre k =
   if k>=0 && k < 3 then 0 else
    begin
      if k >=3 && k < 6 then 3 else 6
    end






(*Test si le nombre place n'est pas deja present dans la région
ma -> matrice a tester 
i j -> coordonnee de la case a tester
va -> nombre a tester*)

    let region ma i j va =
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



(*test en ligne , colonne et region
ma -> matrice a tester 
i j -> coordonnee de la case a tester
va -> nombre a tester*)
let test ma i j va = region ma i j va && testLig ma i j va && testCol ma i j va
