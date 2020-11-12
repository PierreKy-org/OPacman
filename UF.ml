(* Les 2 types utilisables *)

type 't arbre = { valeur : 't; mutable enfant : 't arbre array option  } 

type  't union_find = 't arbre array ;;
 
(* La fusion met l'arbre 2 sous l'arbre 1 *)
let fusion a1 a2 = 
  let rec aux a3 a2 = match a3 with 
  | {valeur ; enfant } when enfant = None -> a3.enfant <- Some ([|a2|])
  | {valeur ; enfant } -> let ran = Random.int ((Array.length (Option.get enfant))-1) in  aux (Option.get enfant).(ran) a2
  in 
  aux a1 a2;
  


(* Opérateur *)

let ( ?. ) a   = {valeur = a; enfant = None}
let ( ** ) a b = fusion a b


(* Ici c'est un peu le crash test *)
let arbretest = {valeur = 8; enfant = Some  ([|{valeur = 5 ; enfant = None};{valeur = 1 ; enfant = None}|])}
let arbretest2 = {valeur = 3; enfant = Some ([|{valeur = 2; enfant = None}|]) }
let arbretest3 = {valeur = 4; enfant = None }

let union = [|arbretest; arbretest2; arbretest3|]


(* Debut d'un pretty printer *)

(*Refaire le printer_arbre car il est horrible x) *)
let rec printer_arbre arbre = match arbre with 
| {valeur ; enfant } when enfant = None -> print_int valeur;
| {valeur ; enfant } -> print_int valeur; print_string " -> ";
                        for i=0 to ((Array.length (Option.get enfant))-1) do
                          printer_arbre (Option.get enfant).(i); if i = ((Array.length (Option.get enfant))-1) then print_string "" else print_string " | "
                          done

let printer_union union = 
  print_string ("Votre tableau est de longueur " ^ string_of_int (Array.length union) ^ "\n");
  for i=0 to ((Array.length union)-1) do
    print_int i; print_string " : "; 
    printer_arbre union.(i);
    print_string "\n"
  done


(* Print l'union, puis fusionne l'arbre 0 et l'arbre 1 puis print le résultat *)
let () =   arbretest ** arbretest2; printer_union union; 