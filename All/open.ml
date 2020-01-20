(*Ouverture de fichier et lecture : close_in ? le i = 'numero de fichier', s ="grids/grid" ou "solutions/solution" *)
let l i s = 
  let l1 = (s ^ Char.escaped i) in 
  let l2 = ( l1^ Char.escaped '.') in 
  let l3  = (l2^ Char.escaped 't')in 
  let l4  = (l3^ Char.escaped 'x') in 
  let l5 = (l4^ Char.escaped 't') in 
  let c = open_in l5 in 
  input_line c 
