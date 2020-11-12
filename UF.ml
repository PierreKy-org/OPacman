(* Les 2 types utilisables *)

type 't arbre = { valeur : 't; mutable parent : 't arbre option} 

type  't union_find = 't arbre array ;;

(* Ici c'est un peu le crash test *)
let arbretest = {valeur = 5; parent = Some  ({valeur = 8 ; parent = None})}
let arbretest2 = {valeur = 2; parent = Some ({valeur = 3; parent = None}) }
let arbretest3 = {valeur = 4; parent = None }

let union = [|arbretest; arbretest2; arbretest3 |]

(* La fusion met l'arbre 1 sous l'arbre 2 *)
let rec fusion a1 a2 = match a1 with 
| {valeur ; parent } when parent = None -> a1.parent <- Some a2;
| {valeur ; parent } -> fusion (Option.get parent) a2

(* Debut d'un pretty printer *)
let rec printer_arbre arbre = match arbre with 
| {valeur ; parent } when parent = None -> print_int valeur
| {valeur ; parent } -> printer_arbre (Option.get parent); print_string " -> "; print_int valeur

let printer_union union = 
  print_string ("Votre tableau est de longueur " ^ string_of_int (Array.length union) ^ "\n");
  for i=0 to ((Array.length union)-1) do
    print_int i; print_string " : "; 
    printer_arbre union.(i);
    print_string "\n"
  done

(*Print l'union, puis fusionne l'arbre 0 et l'arbre 1 puis print le résultat *)
let () =   printer_union union ; fusion union.(0) union.(1); printer_union union; 