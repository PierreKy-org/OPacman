open Graphics
open Sys
open Unix
(* Graphics *)
let rec loop () = loop ()


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
let trace_mur upleftx uplefty taille_case (d,x,y) = 
    moveto (upleftx + (taille_case* (y+1))) (uplefty - (taille_case*(x+1)));
    if d = 1 then
       lineto (upleftx + (taille_case* (y+1))) (uplefty - (taille_case*(x))) 
    else 
            lineto (upleftx + (taille_case* (y))) (uplefty - (taille_case*(x+1)))

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
          loop() 



