open Graphics
open Sys
open Unix
open Thread

(*----------------------------- variables globales *)
let largeur = 10
let hauteur = 10
let taille_par_case = 50
let mur_p = ref (Labyrinthe.gen_lab largeur hauteur)
(* La case où le pacman est actuellement *)
let case_pacman = ref 0
let case_fantome = ref (largeur-1)
(*perdu et gagner permettent la communication entre les 2 threads *)
let perdu = ref 0
let gagner = ref 0 


(*----------------------------- affichage du fantome
* trace le fantome par rapport à case_fantome *)
let trace_fantome uplefty upleftx l h taille_case = 
   let x = !case_fantome / l in
   let y = !case_fantome mod l in
   set_color blue;
   fill_circle ((uplefty + ((taille_case* (y+1))))-(taille_case/2)) ((upleftx - ((taille_case*(x+1))))+(taille_case/2)) ((taille_case/2)-3)

(* trace un rond blanc pour effacer l'emplacement précédent du fantome *) 
let white_fantome uplefty upleftx l h taille_case precedente_case_fantome = 
   let x_pre = precedente_case_fantome / l in
   let y_pre = precedente_case_fantome mod l in
   set_color white;
   fill_circle ((uplefty + ((taille_case*(y_pre+1))))-(taille_case/2)) ((upleftx - ((taille_case*(x_pre+1))))+(taille_case/2)) ((taille_case/2)-3)
  


(*----------------------------- affichage du pacman
 * trace le pacman par rapport à case_pacman *)
let trace_pacman uplefty upleftx l h taille_case = 
   let x = !case_pacman / l in
   let y = !case_pacman mod l in
   set_color yellow;
   fill_circle ((uplefty + ((taille_case* (y+1))))-(taille_case/2)) ((upleftx - ((taille_case*(x+1))))+(taille_case/2)) ((taille_case/2)-3)


(* trace un rond blanc pour effacer l'emplacement précédent de pacman *) 
let white_pacman uplefty upleftx l h taille_case c = 
   let x = !case_pacman / l in 
   let y = !case_pacman mod l in
   set_color white;
   (* c est le l'entrée clavier du joueur *)
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
 


(*----------------------------- Image de victoire et de fin de partie
 * Boucle infinie pour afficher le mot "gagné"
 * On peut mettre un read_key() pour sortir du programme proprement *)
let rec winner_loop() =
   set_color black;
   moveto 300 450;
   set_font "-*-fixed-medium-r-semicondensed--150-*-*-*-*-*-iso8859-1" ;
   draw_string("GAGNE");
   sleep 2

let rec exit_loop() = 
   set_color black;
   moveto 50 450;
   set_font "-*-fixed-medium-r-semicondensed--100-*-*-*-*-*-iso8859-1" ;
   draw_string("Fin de la Partie");
   sleep 2

let rec loser_loop() = 
   set_color black;
   moveto 300 450;
   set_font "-*-fixed-medium-r-semicondensed--100-*-*-*-*-*-iso8859-1" ;
   draw_string("PERDU");
   sleep 2

(*----------------------------- Debut du code de la recherche en profondeur*)
let caseVoisines case = 
  (** int -> int Array 
  Renvoie un tableau de toutes les cases accessibles en un pas depuis
  la case "case". (càd : quand il n'y a aucun mur entre "case" et une case adjacente.)
  Si une case n'est pas accessible (càd : il y'a un mur) alors le tableau
  contiendra la valeur -1*)

  let x = case / largeur in
  let y = case mod largeur in
  let caseVoisine = Array.make 4 (-1) in

  if x <> 0 then 
    if (!mur_p.(0).(x-1).(y)) = false
      then Array.set caseVoisine 0 (y + largeur*(x-1)); (*haut*)

  if y <> (largeur -1) then
    if(!mur_p.(1).(x).(y)) = false 
      then Array.set caseVoisine 1 ((y+1) + largeur*x);   (*droite*)
      
  if y <> 0 then
    if(!mur_p.(1).(x).(y-1)) = false 
      then Array.set caseVoisine 2 ((y-1) + largeur*x); (*gauche*)

  if x <> (hauteur -1) then 
    if(!mur_p.(0).(x).(y)) = false 
      then Array.set caseVoisine 3 (y + largeur*(x+1)); (*bas*)

  caseVoisine
  
let caseVoisinesNonMarquee case caseMarquees  = 
  (** int -> int Array -> int Array
  Renvoie un tableau d'entier contenant toutes les cases voisines de 
  "cases" qui n'ont pas encore été visitées (càd : qui ne sont pas 
  dans déjà caseMarquees) *)

  let  casePossibles = ref [||] in  
  if case < 0
    then [||]
  else begin
    let  cv = (caseVoisines case) in 
    for c=0 to ((Array.length cv)-1)  do
      if (Array.mem (Array.get (cv) c) caseMarquees) = false
        then casePossibles := Array.append !casePossibles [|(Array.get cv c)|];
    done;
    !casePossibles
  end

let prochaine_case_fantome = ref (-1) (*Variable globale qui est modifiée uniquement part est_reliee, indique la case sur laquelle deplacer le fantome *)

let rec est_reliee case caseMarquees caseDepart = 
  (** int -> int Array -> int -> () 
  Parcours les cases adjacentes accessibles et Non marquées jusqu'à trouver la case de pacman.
  Lorsqu'un chemin est trouvé entre pacman et le fantome est_reliee affecte caseDepart à prochaine_case_fantome.
  caseDepart est la valeur de la première case sur laquelle on a appelé est_reliee. 
  Lorsque l'on ne trouve pas la case de Pacman on marque la case courrante afin de ne pas y
  revenir par la suite (c'est fait dans CaseUpdate). 
  Puis on rappelle est_reliee sur les cases adjacentes de cette case.
  *)

  let casePossible = (caseVoisinesNonMarquee case caseMarquees ) in 
  if case = !case_pacman
    then prochaine_case_fantome := caseDepart
    else begin
      let caseUpdate = (Array.append caseMarquees [|case|]) in 
      for c = 0 to ((Array.length casePossible)-1) do
        if case > 0
          then est_reliee (Array.get casePossible c) caseUpdate caseDepart
      done;
    end

let recherche () = 
  (** () -> int
  Initialise les caseMarquees, caseVoisine et CaseDepart
  et appelle est_reliee sur les cases adjacentes au fantome.
  Lorsque l'execution de recherche se termine, la valeur de prochaine_case_fantome 
  aura forcément été modifiée par est_reliee, on la renvoie.
  Recherche sert aussi à indiquer quels sont les cases adjacentes au fantome et
  a initialiser les valeurs pour est_reliee *)

  let caseMarquees =  [|!case_fantome|] in 
  let cv = caseVoisines !case_fantome in 
  for c=0 to ((Array.length cv)-1) do 
    est_reliee ((Array.get cv c)) caseMarquees (Array.get cv c) ;
  done;
  !prochaine_case_fantome



(*----------------------------- Thread fantome*)
let rec thread_fantome ()  = 
   sleep 2;
   (* ici condition pour voir si pacman est sorti du labyrinthe ou pas
    * ca evite qu'une boule bleu pop sur l'ecran de victoire *)
   if !gagner <> 1 then begin 
     let precedente_case_fantome = !case_fantome in
     case_fantome := recherche ();               (*pathfinding*)
     white_fantome 200 800 largeur hauteur taille_par_case precedente_case_fantome; (* on efface l'ancien emplacement*)
     trace_fantome 200 800 largeur hauteur taille_par_case; (* on redessine le fantome a son nouveau emplacement *)
     (*test si le fantome et le pacman se touchent *)
     if !case_pacman = !case_fantome then begin
       perdu := 1;
       (* ici je met tout les murs sur chaque case car dans loop on s'arrete sur l'entrée clavier
        * ca evite d'avoir une boule jaune apparaitre sur l'ecran de fin à cause d'un déplacement *)
       mur_p := (Labyrinthe.initialise_mur_present largeur hauteur);
       clear_graph();
       loser_loop()
     end
     else     
       thread_fantome ()
   end



(*----------------------------- Boucle principale + pacman*)
let fct_graphique_pacman l h c =
    white_pacman 200 800 l h taille_par_case c;
    trace_pacman 200 800 l h taille_par_case 
 
let rec loop l h = 
  (* Si le pacman rentre dans le fantome  *)
    if !case_pacman = !case_fantome 
      then begin
       clear_graph();
       gagner := 1;
       loser_loop()
      end
    else
      (* si le pacman atteint la fin du labyrinthe *)
      if !case_pacman = l*h-1
        then begin
         clear_graph();
         gagner := 1;
         winner_loop()
        end
      else                   
        let c = read_key() in 
    
        (* si il ne perd pas alors on utilise l'entrée clavier *)
        let x = !case_pacman / l in
        let y = !case_pacman mod l in
        (* dans l'ordre :
           * - calcule la position du pacman après la pression d'une touche
           * - efface l'ancien pacman 
           * - affiche le nouveau pacman *)
        match c with 
        | w when w = 'z' ->  
          if ((x>0) && (!mur_p.(0).(x-1).(y) = false))
            then begin
             case_pacman := (y + l*(x-1)); 
             fct_graphique_pacman l h c
            end
          else 
            sound 12000 1000;
          loop l h

        | w when w = 'q' ->  
          if ((y>0)&& (!mur_p.(1).(x).(y-1) = false))  
            then begin
             case_pacman := (y-1) + l*x;
             fct_graphique_pacman l h c
            end
          else  
            sound 12000 100;
          loop l h
              
        | w when w = 'd' ->  
          if (!mur_p.(1).(x).(y) = false)
            then begin
             case_pacman := (y+1) + l*x;
             fct_graphique_pacman l h c
            end
          else
            sound 12000 1000; 
          loop l h

        | w when w = 's' -> 
          if (!mur_p.(0).(x).(y) = false)
            then begin
             case_pacman := (y + l*(x+1));
             fct_graphique_pacman l h c
            end
          else 
            sound 12000 1000;
          loop l h
        (* Quitte le jeu sans fermer la fenêtre (comme dans l'ennoncé) *) 
        | w when w = 'e' -> gagner := 1; clear_graph(); exit_loop()
        (*Empeche de crash si on appuie sur autre chose *)
        | w -> loop l h 

(*----------------------------- Affichage du labyrinthe*)

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
    for y = 0 to (l-1) do
      for x = 0 to (h-1) do
        if (mur_present.(0).(x).(y)) = true 
         then trace_mur upleftx uplefty taille_case (0,x,y);
        if (mur_present.(1).(x).(y)) = true 
         then trace_mur upleftx uplefty taille_case (1,x,y);
        done;
      done;
    trace_pourtour upleftx uplefty l h taille_case;
    end


(*----------------------------- Lancement du programme*)

(* Lance le thread du fantome *)
let _ = create thread_fantome()

let () = 
  trace_lab 200 800 taille_par_case largeur hauteur !mur_p;
  trace_pacman 200 800 largeur hauteur taille_par_case;
  trace_fantome 200 800 largeur hauteur taille_par_case ;
  loop largeur hauteur
