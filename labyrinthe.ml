  (* CASE ADJACENTES V2 l'algo calcule la position comme demande le prof
   int  -> int -> (int,int,int) -> (int,int) *)
  let case_adjacentesbis l h (d,x,y) = 
    match (d,x,y) with 
    | (w,x,y) when w  = 0 -> if x >= (l-1) then invalid_arg "limite du labyrinthe" else (y+l*x, y+l*(x+1))
    | (w,_,_) -> if y >= (h-1) then invalid_arg "limite du labyrinthe" else (y+l*x, (y+1)+l*x)
  
  (*Generation d'un matrice 2D 5x5 *)
    let cas = case_adjacentesbis 5 5 (0,0,2) (* OK *)

    (* Test de l'implentation du module *)
    let test = UF.create 10

    (* Print de case adjacente *)
    let () = print_int (fst cas); print_int (snd cas)