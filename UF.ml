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
->  Refaire tout uf pour avoir la bonne structure ???           DONE ?????
*)

type 't uf = 't array

let is_a_in_compo a elem uf  = 
  (** int -> 't  -> uf -> Boolean 
  Indique si la case a a comme racine elem dans l'union-find uf *)
  if uf.(a) = elem then true else false

let create n  =  
  (* int -> uf
  Créer une UNION FIND de n 't éléments.
  À utiliser pour initialiser le labyrinthe*)
  let tab = Array.make n 0 in
  for i=0 to ((Array.length tab)-1) do 
    tab.(i) <- i
  done;
  tab

let find uf a = 
  (** uf -> int -> int
  Renvoie la racine de la case a dans toutes les composantes de uf.*)
  uf.(a)

let rec count uf n = 
  (* uf -> int -> int 
  count le nombre d'occurence de n dans une liste uf 
  on utilise cette fonction dans union avec Array.to_list *)
  match uf with
  | [] -> 0
  | [x] -> if x = n then 1 else 0
  | hd::tl -> if hd = n then 1 + count tl n  else count tl n

let rec same_rac uf n acc inc =
   match uf with
   | [] -> acc
   | [x] -> if x = n then acc@[inc] else acc
   | hd::tl -> if hd = n then same_rac tl n (acc@[inc]) (inc+1) else same_rac tl n acc (inc+1)


let union uf n m = 
  (* uf -> int -> int -> uf
  La classe d'une case est un 't array
  Renvoie une nouvelle uf dans la quelle les classes des cases n et m sont 
  fusionnées.*)
  if (find uf n ) != (find uf m )
    then begin
    let nb_n = count (Array.to_list uf) uf.(n) in
    let nb_m = count (Array.to_list uf) uf.(m) in 
    let list_n = (Array.of_list (same_rac (Array.to_list uf) uf.(n) [] 0)) in
    let list_m = (Array.of_list (same_rac (Array.to_list uf) uf.(m) [] 0)) in
    if nb_n < nb_m then 
      for i= 0 to ((Array.length list_n)-1) do      
        uf.(list_n.(i)) <- uf.(m)
      done
    else 
      for i = 0 to ((Array.length list_m)-1) do
      uf.(list_m.(i)) <- uf.(n);
      done;
    uf
    end
  else
  uf

let printerUF uf = 
  (**uf -> unit
  Affiche une UF
  Utilisée pour le debuggage uniquement*)
  let rec print_list = function 
  [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l in
  print_list (Array.to_list uf)

(* --------------------------------Partie Tests-------------------------------- *)
(*
let uf1 = create 10

let testFind1 = find uf1 1      (* OK *)
let testFind2 = find uf1 4      (* OK *)
let testFind3 = find uf1 7      (* OK *)



let testUnion1 = union uf1 3 5      (* OK *)
let testUnion2 = union uf1 1 3      (* OK *)
let testUnion3 = union uf1 5 8      (* OK *)

let testUnion4 = union uf1 8 6      (* OK *)

let testUnion5 = union uf1 0 1      (* OK *)

let testUnion6 = union uf1 1 2      (* OK *)

let testUnion7 = union uf1 3 4      (* OK *)

let testUnion8 = union uf1 6 7      (* OK *)

let testUnion9 = union uf1 9 8      (* OK *)

let testFind1 = find uf1 1      (* OK *)
let testFind2 = find uf1 4      (* OK *)
let () = print_string("---------- TEST MODULE UF ---------\n");printerUF uf1; print_string ("\n"); print_int testFind1; print_string ("\n") 
*)
