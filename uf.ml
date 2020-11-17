
(*TODO 
    Creer UF                                                    DONE
      -Create                                                   DONE
      -Find                                                     DONE
      -Union                                                    DONE
    Adapter le printer au type UF                               DONE
    Enlever le type Option                                      DONE
    Embellir et Clarifier TOUT le code                          DONE
    Commenter                                                   DONE
    Gérer Bug dans Union, on ne renvoit                         DONE
      pas toute les compo après union                          
    Gérer le cas où on fait union de 2 cases de même classes    DONE
->  Refaire tout uf pour avoir la bonne structure ???           A CONSIDERER
*)

type composante_Connexe = {racine : int; enfants : int array} 
type uf = {composantes: composante_Connexe array }

let is_a_in_compo a compo = 
  (** int -> composante_Connexe -> Boolean 
  Indique si la case a est dans la composante compo. *)
  Array.mem a (compo.enfants)

let fill n = 
  (** int -> composante_Connexe array
  Renvoie un tableau de taille n de composante racines(racine i; valeur i)*)
  let tabComposantes = Array.make n {racine=0;enfants = [||]} in
    for i = 0 to (n-1) do 
      Array.set tabComposantes i {racine = i; enfants = [|i|]}
    done;
    tabComposantes

let create n = 
  (** int -> uf
  Utilise fill pour créer une UNION FIND de composantes racines.
  À utiliser pour initialiser le labyrinthe*)
  let uf = {composantes = (fill n)} in
    uf

let find uf a = 
  (** uf -> int -> int
  Renvoie la racine de la case a dans toutes les composantes de uf.*)
  let rac = ref (-1) in 
  for i = 0 to (Array.length (uf.composantes)-1) do 
    if is_a_in_compo a uf.composantes.(i)
    then rac := uf.composantes.(i).racine
    else rac := !rac;
  done;
  !rac


let fusion uf n m = 
  (** uf -> int -> int -> composante_Connexe
  La classe d'une case est un int array
  Cherche les racines de n et m dans uf
  Utilise les racines pour fusionner les classes de n et m en une nouvelle
    composantes connexe*)
  let aux t1 t2 =
    (** int array -> int array -> int array
    Concatène les tableaux t1 et t2.
    Est utilisé pour fusionner les enfants des composantes connexes de 
      classe n et m*)
  let resultat = ref ((Array.make ((Array.length t1) + (Array.length t2)) 0)) in
  let compteur = ref 0 in 
  for i = 0 to ((Array.length t1) -1) do 
    Array.set (!resultat) (!compteur) (t1.(i));
    compteur := !compteur+1;
  done;
  for i = 0 to ((Array.length t2) -1) do 
    Array.set (!resultat) (!compteur) (t2.(i));
    compteur := !compteur+1;
  done;
  !resultat in 

  let doNothing = ref 0 in 
  let racineN = find uf n in 
  let racineM = find uf m in 
  let tabComposante1 = ref [||] in 
  let tabComposante2 = ref [||] in 
  for i=0 to ((Array.length uf.composantes) -1)do
    if uf.composantes.(i).racine = racineN
    then tabComposante1 := uf.composantes.(i).enfants
    else 
    if uf.composantes.(i).racine = racineM
    then tabComposante2 := uf.composantes.(i).enfants
    else doNothing := !doNothing 
  done;
  {racine = racineN; enfants = (aux !tabComposante1 !tabComposante2)} 

let union uf n m = 
  (** uf -> int -> int -> uf
  La classe d'une case est un int array
  Renvoie une nouvelle uf dans la quelle les classes des cases n et m sont 
  fusionnées.
  *)
  let tailleUF = (Array.length (uf.composantes)) in
  let tabComposanteUnies = ref (fill (tailleUF -1)) in 
  let compteur = ref 0 in
  if (find uf n ) != (find uf m )
  then begin
  for i=0 to (tailleUF-1) do
    (* Si une composante n'a pas pour racine n ou m on l'ajoute dans 
        le nouveau tableau*)
    if uf.composantes.(i).racine != (find uf n) && uf.composantes.(i).racine != (find uf m)
    then begin 
      !tabComposanteUnies.(!compteur) <- uf.composantes.(i);
      compteur := !compteur+1;
    end
    else compteur := !compteur
  done;
  (* On crée la fusion des composantes n et m et on l'ajoute à la 
    fin du nouveau tableau.
    Puis on renvoit une uf contenant le tableau totalement unifié. *)
  let union_de_N_et_M = (fusion uf n m) in
  (!tabComposanteUnies).(tailleUF-2) <- union_de_N_et_M;
  {composantes = !tabComposanteUnies}
  end
  else 
  uf

let printerUF uf = 
  (**uf -> unit
  Affiche une UF
  Utilisée pour le debuggage uniquement*)
let printerComposanteEnfants compo =
  for i=0 to ((Array.length compo)-1) do 
    print_int compo.(i);print_string "; ";
  done;
  in 
  print_string "\n";
  for i=0 to ((Array.length uf.composantes)-1) do
    print_int uf.composantes.(i).racine; print_string " -> "; 
    printerComposanteEnfants (uf.composantes.(i).enfants);
    print_string "\n"
  done;
  print_string "fin\n"

(* --------------------------------Partie Tests-------------------------------- *)

let testCreate = create 10
let compo1 = {racine = 0; enfants = [|1;2;3|]}  
let compo2 = {racine = 1; enfants =  [|4;5;6|]}  
let compo3 = {racine = 2; enfants =  [|7;8|]} 
let compoArray = [|compo1;compo2;compo3|]
let construireUF tabCompo = {composantes = tabCompo}
let uf1 = construireUF compoArray

let testFind1 = find uf1 1      (* OK *)
let testFind2 = find uf1 4      (* OK *)
let testFind3 = find uf1 7      (* OK *)
let testFind4 = find uf1 999    (* OK *)

let testUnion1 = union uf1 3 5      (* OK *)
let testUnion2 = union uf1 1 1      (* OK *)
let testUnion3 = union uf1 5 8      (* OK *)

let test1 =  printerUF testCreate
let test2 =  print_int testFind4
let test3 =  printerUF testUnion2
