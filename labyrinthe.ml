open Format

let case_adjacentesbis l h (d,x,y) = 
(** int -> int -> int * int * int -> int * int
L'algo calcule la position. 0 donne le trait horizontal et 1 le trait vertical
*)
  match (d,x,y) with 
  | (w,x,y) when w  = 0 -> if x >= (l-1) then invalid_arg "limite du labyrinthe" else (y+l*x, ((y)+l*(x+1)))
  | (w,_,_) -> if y >= (h-1) then invalid_arg "limite du labyrinthe" else (y+l*x, ((y+1)+l*(x)))

(*Fonctions pour manipuler des tuples *)
let getTuple2First (a,b)= a 
let getTuple2Second (a,b) =  b
let getTuple3First (a,b,c) = a
let getTuple3Second (a,b,c) = b
let getTuple3Last (a,b,c) = c

let initialise_mur_present l h = 
  Array.init 2 (fun _ -> (Array.init l (fun _ -> Array.make h true)))

let mur_au_hasard l h =
  (**int -> int -> int * int * int
  renvoie un triplet (d, x, y)
  *)
  let n = Random.self_init (); Random.int ((l-1) * h + l * (h-1)) in
  if n < (l-1) * h
    then begin
     (1, n mod l, n / l) 
    end
  else
    let n2 = n - (l-1) * h in
    (0, n2 mod (l-1), n2 / (l-1))

let gen_lab l h = 
  (* int -> int -> bool bool bool Array
  Creer un labyrinthe de dimension l h sous forme de tableau de tableau de tableau de booleans.
  Le premier tableau correspond à d  -> debout ou allongé
  Le deuxieme tableau correspond à x
  Le dernier tableau correspond aux y
  *)
  let mur_present = ref (initialise_mur_present l h) in 
  let uf = ref (UF.create (l*h)) in
  let dxy = ref (0,0,0) in
  let ij = ref (0,0) in 
  let incrementeur = ref 1 in 

  while !incrementeur <= ((l*h)-1) do
    dxy := (mur_au_hasard l h);
    ij := case_adjacentesbis l h !dxy;
    if (UF.find !uf (getTuple2First !ij)) = (UF.find !uf (getTuple2Second !ij))
      then begin  
       incrementeur := !incrementeur
      end
    else begin 
      uf := (UF.union !uf (getTuple2First !ij) (getTuple2Second !ij));
      !mur_present.(getTuple3First(!dxy)).(getTuple3Second(!dxy)).(getTuple3Last(!dxy)) <- false;
      incrementeur := !incrementeur+1;
    end
  done;
  !mur_present

let printerMurPresent t l h = 
  (**int -> int -> int  
  Uniquement pour debugger.
  *)
  for d=0 to 1 do 
    for x=0 to l-1 do
      for y=0 to h-1 do
        printf "%b " (t.(d).(x).(y));
      done;
        printf "\n"
    done;
    printf "\n"
  done;
  printf "fin\n"

(* --------------------------------Partie Tests-------------------------------- *)

(* Test de l'implentation du module *)
(* let test = (UF.create 10) *)
(*Generation d'un matrice 2D 5x5 *)
(*let testCaseAdj = case_adjacentesbis 5 5 (0,0,2) (* OK *)

let testCaseAdj2 = case_adjacentesbis 5 5 (1,0,2) (* OK *) *)
(*let testMurPresent = initialise_mur_present 3 3
*)

(*let () = printerMurPresent testGenLab1 3 3
*)

(* Print de case adjacente *)

(* Test de Mur Present & GenLab *)
(* let test2 = printerMurPresent testMurPresent 3 3  *)
(*
let () = print_string("--------------TEST MODULE LABYRINTHE -------------\n"); 
let zizi = gen_lab 5 5 in
 print_int (fst testCaseAdj); print_int (snd testCaseAdj); print_string ("\n");
  print_int (fst testCaseAdj2); print_int (snd testCaseAdj2); print_string ("\n")  *)
