(*
TODO : 
    Importer le package UF.ml                       DONE
    Pseudo Coder gen_lab                            DONE
    Implementer les fonctions annexes de gen_lab    DONE
      initaliserMur                                 DONE
      choisirMurAleatoire                           DONE
    Implementer une v1 du generateur de labyrinthe  DONE
->  Corriger Bug : le labyrinthe n'a aucun mur      DONE
    
  *)
open Format

  let case_adjacentesbis l h (d,x,y) = 
  (** CASE ADJACENTES V2 l'algo calcule la position comme demande le prof
   int  -> int -> (int,int,int) -> (int,int) *)
    match (d,x,y) with 
    | (w,x,y) when w  = 0 -> if x >= (l-1) then invalid_arg "limite du labyrinthe" else (y+l*x, y+l*(x+1))
    | (w,_,_) -> if y >= (h-1) then invalid_arg "limite du labyrinthe" else (y+l*x, (y+1)+l*x)

let getTuple2First (a,b)= a 
let getTuple2Second (a,b) =  b
let getTuple3First (a,b,c) = a
let getTuple3Second (a,b,c) = b
let getTuple3Last (a,b,c) = c

let printerMurPresent t l h = 
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


let initialise_mur_present l h = 
  
  Array.init 2 (fun _ -> (Array.init l (fun _ -> Array.make h true)))

let mur_au_hasard l h =(* renvoie un triplet (d, x, y) *)
  let n = Random.int ((l-1) * h + l * (h-1)) in
  if n < (l-1) * h
  then (0, n mod (l-1), n / (l-1))
  else
  let n2 = n - (l-1) * h in
    (1, n2 mod l, n2 / l)

let gen_lab l h = 
  let mur_present = ref (initialise_mur_present l h) in 
  let uf = ref (UF.create (l*h)) in
  let dxy = ref (0,0,0) in
  let ij = ref (0,0) in 
  let incrementeur = ref 1 in 

  while !incrementeur < ((l*h)-1) do
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


  
(*Pseudo Code*)
(*
let initialise_mur_present l h =
  0
let mur_aleatoire mur_present = 
  0

let gen_lab l h = 
  let mur_present = ref (initialise_mur_present l h) in 
  let uf = ref (Uf.find (l*h)) in
  for i = 0 to ((l*h)-1) 
    let (d,x,y) = mur_aleatoire mur_present
    let (i,j) case_adjacentes l h (d,x,y)
    if (UF.find i) = (UF.find 
    then i := !i-1
    else begin 
      uf := (Uf.union i j );
      mur_present.(m.(0)).(m.(1)).(m(2)) <- false;
    end
  done;
  !mur_present
*)
(* --------------------------------Partie Tests-------------------------------- *)

(* Test de l'implentation du module *)
let test = (UF.create 10)
(*Generation d'un matrice 2D 5x5 *)
let testCaseAdj = case_adjacentesbis 5 5 (0,0,2) (* OK *)
let testMurPresent = initialise_mur_present 3 3

let testGenLab1 = gen_lab 3 3

let () = printerMurPresent testGenLab1 3 3
(* Print de case adjacente *)
(* let test1 = print_int (fst testCaseAdj); print_int (snd testCaseAdj) *)

(* Test de Mur Present & GenLab *)
(* let test2 = printerMurPresent testMurPresent 3 3  *)
