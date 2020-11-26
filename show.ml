open Graphics

(* Graphics *)
let rec loop () = loop ()

let trace_pourtour upleftx uplefty l h taille_case = begin 
  set_color red; 
  draw_rect (upleftx) ((uplefty-((taille_case*h)))) (taille_case*l)  (taille_case*h) ;
  end

let trace_mur upleftx uplefty taille_case (d,x,y) = 
    moveto upleftx uplefty;
    if d = 0 then
       lineto (upleftx+taille_case) uplefty
    else 
       lineto upleftx (uplefty+taille_case)
(* Début de trace_lab *)
let trace_lab upleftx uplefty taille_case l h mur_present = begin
    open_graph " 900x900";
    trace_pourtour upleftx uplefty l h taille_case
    end

let mur_p = Labyrinthe.gen_lab 10 10
let () =  trace_lab 200 400 20 10 10 mur_p; trace_mur 20 20 20 (1,1,1); loop() 



