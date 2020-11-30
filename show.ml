open Graphics
open Sys
open Unix
(* Graphics *)



let mur_p = ref (Labyrinthe.gen_lab 10 10)
(* La case où le pacman est actuellement *)
let case_pacman = ref 0


(* trace le pacman par rapport à case_pacman *)
let trace_pacman uplefty upleftx l h taille_case  = 
  let x = !case_pacman / l in
  let y = !case_pacman mod l in
  set_color yellow;
  fill_circle ((uplefty + ((taille_case* (y+1))))-(taille_case/2)) ((upleftx - ((taille_case*(x+1))))+(taille_case/2)) ((taille_case/2)-3)


(* trace un rond blanc pour effacer l'emplacement précédent de pacman *) 
let white_pacman uplefty upleftx l h taille_case c = 
   let x = !case_pacman / l in 
   let y = !case_pacman mod l in
   set_color white;
  match c with 
   | w when w = 'z' ->  fill_circle ((uplefty + ((taille_case* (y+1))))-(taille_case/2)) ((upleftx - ((taille_case*(x+2))))+(taille_case/2))
     ((taille_case/2)-3)
   | w when w = 'q' ->  fill_circle ((uplefty + ((taille_case* (y+2))))-(taille_case/2)) ((upleftx - ((taille_case*(x+1))))+(taille_case/2))
     ((taille_case/2)-3)  
   | w when w = 'd' -> fill_circle ((uplefty + ((taille_case* (y))))-(taille_case/2)) ((upleftx - ((taille_case*(x+1))))+(taille_case/2))
     ((taille_case/2)-3)
   | w when w = 's' -> fill_circle ((uplefty + ((taille_case* (y+1))))-(taille_case/2)) ((upleftx - ((taille_case*(x))))+(taille_case/2))
     ((taille_case/2)-3)
   (* Ne fais rien si une autre touche est enfoncée *)
   | w -> case_pacman := !case_pacman 
 

(* Boucle infinie pour afficher le mot "gagné"
 * On peut mettre un read_key() pour sortir du programme proprement *)
let rec winner_loop() =
  set_color black;
  moveto 300 450;
  set_font "-*-fixed-medium-r-semicondensed--150-*-*-*-*-*-iso8859-1" ;
  draw_string("GAGNE");
  winner_loop() 
            
let rec exit_loop() = 
  set_color black;
  moveto 50 450;
  set_font "-*-fixed-medium-r-semicondensed--100-*-*-*-*-*-iso8859-1" ;
  draw_string("Fin de la Partie");
  exit_loop() 

(* Boucle infinie qui lance l'affichage + la lecture d'entrée clavier *) 
let rec loop l h = 
 if !case_pacman = l*h-1 then begin
   clear_graph();
   winner_loop()
   end
 else                
   let c = read_key() in 
   let x = !case_pacman / l in
   let y = !case_pacman mod l in
   (* dans l'ordre :
         * - calcule la position du pacman après la pression d'une touche
         * - efface l'ancien pacman 
         * - affiche le nouveau pacman *)
   match c with 
     | w when w = 'z' ->  
      if (x>0) then
          if(!mur_p.(0).(x-1).(y) = false) then
             begin
             case_pacman := (y + l*(x-1)); 
             white_pacman 200 800 10 10 50 c; 
             trace_pacman 200 800 10 10 50 ; 
             loop l h;
             end
          else  
             begin
              sound 12000 1000;
              loop l h
             end
      else 
          sound 12000 1000;
          loop l h
          
     | w when w = 'q' ->  
      if (y>0) then 
        if(!mur_p.(1).(x).(y-1) = false) then begin
           case_pacman := (y-1) + l*x;
           white_pacman 200 800 10 10 50 c;
           trace_pacman 200 800 10 10 50 ;
           loop l h;
           end
        else  
           begin
           sound 12000 100;
           loop l h
           end
      else 
        sound 12000 1000;
        loop l h
      

     | w when w = 'd' ->  if(!mur_p.(1).(x).(y) = false) then
      begin
      case_pacman := (y+1) + l*x;
      white_pacman 200 800 10 10 50 c;
      trace_pacman 200 800 10 10 50 ;
      loop l h;
      end
      else
          begin
          sound 12000 1000; 
          loop l h
          end
     | w when w = 's' -> if(!mur_p.(0).(x).(y) = false) then
      begin
      case_pacman := (y + l*(x+1));
      white_pacman 200 800 10 10 50 c;
      trace_pacman 200 800 10 10 50 ;
      loop l h;
      end
      else 
        begin
          sound 12000 1000;
          loop l h 
        end
     (* Quitte le jeu sans fermer la fenêtre (comme dans l'ennoncé) *) 
     | w when w = 'e' -> clear_graph(); 
                         exit_loop();

     (* Permet de quitter le jeu *)
     | w when w = 'n' -> case_pacman := !case_pacman
   (*Empeche de crash si on appuie sur autre chose *)
     | w -> loop l h 

 

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

let () =  
          (Labyrinthe.printerMurPresent !mur_p 10 10) ;
          trace_lab 200 800 50 10 10 !mur_p;
          trace_pacman 200 800 10 10 50;
loop 10 10 



