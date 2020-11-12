(* Les 2 types utilisables *)
type 't arbre =
| None
| Feuille of 't * 't arbre

type  't union_find = {tab : 't arbre array  };;

 (* Ici c'est un peu le crash test *)
let arbre1 = Feuille( 5 , Feuille(3, Feuille(2 , None)))

let arbre2 = Feuille( 6 , Feuille(8, Feuille(4 , None)))

let unionfind = {tab = [|arbre1;arbre2|]} 


(* Debut d'un pretty printer *)
let rec printer arbre = match arbre with 
| None -> print_string " "
| Feuille(x,y) when y = None -> print_int x
| Feuille(x,y) -> print_int x; print_string " -> ";  printer y


let () = printer arbre1 
