open Graphics

(* Graphics *)
let rec loop () = loop ()

let trace_pourtour upleftx uplefty l h taille_case = begin 
  set_color red; 
  draw_rect (upleftx) ((uplefty-((taille_case*h)))) (taille_case*l)  (taille_case*h) ;
  end

let trace_mur upleftx uplefty taille_case (d,x,y) = 
    moveto (upleftx + (taille_case* (x+1))) (uplefty - (taille_case*(y+1)));
    if d = 1 then
       lineto (upleftx + (taille_case* (x))) (uplefty - (taille_case*(y+1)))
    else 
       lineto (upleftx + (taille_case* (x+1))) (uplefty - (taille_case*(y)))

(* Début de trace_lab *)
let trace_lab upleftx uplefty taille_case l h mur_present = begin
    open_graph " 900x900"; 
    let doNothing = ref 0 in 

    trace_pourtour upleftx uplefty l h taille_case;
      for x = 0 to (l-1) do
        for y = 0 to (h-1) do
          if (mur_present.(0).(x).(y)) = true 
          then trace_mur upleftx uplefty taille_case (0,x,y)
          else doNothing := !doNothing;
          if (mur_present.(1).(x).(y)) = true 
          then trace_mur upleftx uplefty taille_case (1,x,y)
          else doNothing := !doNothing;
          done;
        done;
      end

let mur_p = Labyrinthe.gen_lab 10 10
let () =  trace_lab 200 600 50 10 10 mur_p;
          loop() 



