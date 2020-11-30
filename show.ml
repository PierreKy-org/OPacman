open Graphics
open Sys
open Unix
(* Graphics *)



let case_pacman = ref 0

let trace_pacman uplefty upleftx l h taille_case  = 
  let x = !case_pacman / l in
  let y = !case_pacman mod l in
 (* set_color white;
  match c with 
  | w when w = 'z' -> fill_circle ((uplefty + ((taille_case* (y))))-(taille_case/2)) ((upleftx - ((taille_case*(x+1))))+(taille_case/2))
     ((taille_case/2)-3);
  | w when w = 'q' ->
  | w when w = 'd' ->
  | w when w = 's' -> *)
     set_color yellow;
  fill_circle ((uplefty + ((taille_case* (y+1))))-(taille_case/2)) ((upleftx - ((taille_case*(x+1))))+(taille_case/2))
     ((taille_case/2)-3)
 
let rec loop l = 
 let c = read_key() in 
 let x = !case_pacman / l in
 let y = !case_pacman mod l in
 match c with 
   | w when w = 'z' -> case_pacman := (y + l*(x-1)); print_int(!case_pacman) ;print_string("\n"); trace_pacman 200 800 10 10 50 ;loop l
   | w when w = 'q' -> case_pacman := (y-1) + l*x; print_int(!case_pacman) ; print_string("\n");trace_pacman 200 800 10 10 50 ;loop l
   | w when w = 'd' -> case_pacman := (y+1) + l*x; print_int(!case_pacman) ; print_string("\n");trace_pacman 200 800 10 10 50 ;loop l
   | w when w = 's' -> case_pacman := (y + l*(x+1)); print_int(!case_pacman) ; print_string("\n");trace_pacman 200 800 10 10 50 ;loop l
 

 

(* 0 bug elle marche nickel, construit le pourtour du labyrinthe et affiche l'entrée et la sortie du labyrinthe*)
let trace_pourtour upleftx uplefty l h taille_case = begin 
  set_color black; 
  draw_rect (upleftx) ((uplefty-((taille_case*h)))) (taille_case*l)  (taille_case*h) ;
  moveto (upleftx) (uplefty);
  set_color white; 
  lineto upleftx (uplefty-taille_case);
  moveto (upleftx  + (taille_case * (l) )) (uplefty - (taille_case * (h-1))); 
  lineto (upleftx  + (taille_case * (l) ))  (uplefty - (taille_case * (h)));
  end
(* Parfait, elle fait son taff, construit le mur demander dans le labyrinthe *)
let trace_mur uplefty upleftx taille_case (d,x,y) = 
    moveto (uplefty + (taille_case* (y+1))) (upleftx - (taille_case*(x+1)));
    if d = 1 then
       lineto (uplefty + (taille_case* (y+1))) (upleftx - (taille_case*(x))) 
    else 
            lineto (uplefty + (taille_case* (y))) (upleftx - (taille_case*(x+1)))

(* Début de trace_lab, trace le labyrinthe en suivant la matrice de mur, trace d'abord les murs puis le pourtour *)
let trace_lab upleftx uplefty taille_case l h mur_present = begin
    open_graph " 900x900"; 
    let doNothing = ref 0 in 

      for y = 0 to (l-1) do
        for x = 0 to (h-1) do
          if (mur_present.(0).(x).(y)) = true 
          then trace_mur upleftx uplefty taille_case (0,x,y)
          else doNothing := !doNothing;
          if (mur_present.(1).(x).(y)) = true 
          then trace_mur upleftx uplefty taille_case (1,x,y)
          else doNothing := !doNothing;
          done;
        done;
    trace_pourtour upleftx uplefty l h taille_case;
    end

let mur_p = Labyrinthe.gen_lab 10 10
let () =  trace_lab 200 800 50 10 10 mur_p;
          trace_pacman 200 800 10 10 50;
loop 10 



