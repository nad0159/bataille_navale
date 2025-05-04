(**
Dans ce document est défini le projet à
rendre en fin de semestre pour l'année de {i L1
2024-2025}. Le programme est un jeu de bataille navale.
@version 2
@author Bentz POLO
@author Nadia MOUACHA
@author Zeneibou NIANG
@author Younes ETTABAA
*)
(* open CPgraphics *)
(* Conventions d'écritures:
  - Les fonctions qui proviennent de l'extérieur doivent etre
  précédées du nom de la bibliothèque d'origine. ex: "CPgraphics.open_graph()"
  - La declaration de types se fait dans la section "Types" ;
  celle des fonctions, dans la section "Functions".
  - Les paramètres doivent porter le préfixe "p_" ; et les variables locales, le préfixe "l_"
  - Chacun documente sa fonction au moment de l'implémenter

  Infos:
    - Les commandes pour l'interpréteur sont rassemblées dans "inter.ml".
  Vous pouvez simplement écrire '#use "inter.ml";;' quand vous tester
  vous fonctions dans l'interpréteur.
  *)


(** <h1> Types </h1> *)
(** Type contenant les variables globales de
l'environnement de jeu. Prend en compte la marge
@since version 1
*)
type t_params = {
  margin : int ref; (* Paramètre réglant la marge entre la bordure d'écran et l'espace de jeu *)
  cell_size : int ref; (* Paramètre réglant la taille des cellules des grilles *)
  message_size : int ref; (* Paramètre réglant la taille de la zone d'affichage de message *)
  grid_size : int ref; (* Paramètre réglant la taille, en cellules, des grilles. *)
  ship_sizes : ((string * int) list) ref; (* Liste des bateaux et leurs tailles *)
} ;;

(** Type représentant l'état des cellules
 @since version 2 *)
type t_state = EMPTY | OCCUPIED | CLICKED | TOUCHED | DESTROYED ;;

(** Type représentant les cellules 
 @since version 2 *)
type t_cell = {
  coord : char * int;
  state : t_state ref;
} ;;

(** Type représentant les grilles
 @since version 2 *)
type t_grid = t_cell array array ;;

(** Type représentant un bateau
 @since version 2 *)
type t_ship = {
  name : string;
  positions : (char * int) list;
} ;;
(** Type pour représenter l'orientation du bateau *)
type t_direction = UP | DOWN | LEFT | RIGHT ;;

(* Type pour déterminer la grille ou le joueur a cliqué
 @since version 3 *)
type t_where = ORDINATEUR | JOUEUR | NONE ;;

(* Définition du type structuré t_battleship
 @since version 3 *)
type t_battleship = {
  player_grid : t_grid;  (* Grille du joueur *)
  computer_grid : t_grid;  (* Grille de l'ordinateur *)
  player_ships : t_ship list;  (* Liste des bateaux du joueur *)
  computer_ships : t_ship list;  (* Liste des bateaux de l'ordinateur *)
} ;;

(** <h1>Functions</h1> *)
(**
Initialise les paramètres du jeu.
@param p_margin valeur de la marge, en pixels.
@param p_cell_size taille des cellules de grille, en pixels.
@param p_message_size hauteur de la zone d'affichage de message, en pixels.
@param p_grid_size taille de la grille, en cellules.
@param p_param structure contenant les paramètres du jeu.
@param ship_sizes Liste contenant des couples (bateau, taille)
@author Zeinebou NIANG
@since version 1
*)
let init_params(p_margin, p_cell_size, p_message_size, p_grid_size, p_ship_sizes, p_params : int * int * int * int * (string * int) list * t_params) : unit =
  p_params.margin := p_margin;
  p_params.cell_size := p_cell_size;
  p_params.message_size := p_message_size;
  p_params.grid_size := p_grid_size;
  p_params.ship_sizes := p_ship_sizes;
;;

(**
Fonction auxiliaire à {!val:display_empty_grids}. Dessine une grille et son système de coordonnées.
@param p_x coordonnée x du point inférieur gauche de la grille.
@param p_y coordonnée y du point inférieur gauche de la grille.
@param p_cell_size taille, en pixel, des cellules.
@param p_size taille, en cellules, de la grille.
@author Nadia MOUACHA
@since version 1
*)
let grid_displayer (p_x, p_y, p_cell_size, p_size : int * int * int * int) : unit =
  let l_x : int ref = ref (p_x + p_cell_size)
  and l_y : int ref = ref p_y
  in
  (* Lignes horizontales et les chiffres *)
  for i=p_size downto 0 do
    if i = 0 then (* Si i=0 on trace simplement la ligne et on n'affiche pas de coordonnée *)
      (CPgraphics.moveto(!l_x, !l_y);
        CPgraphics.lineto(!l_x + (p_cell_size * p_size), !l_y))
    else
      (CPgraphics.moveto(p_x, !l_y);
        CPgraphics.draw_string(string_of_int(i));
        CPgraphics.moveto(!l_x, !l_y);
        CPgraphics.lineto(!l_x + (p_cell_size * p_size), !l_y));
    l_y := !l_y + p_cell_size;
  done;

  l_y := p_y ; (* Réinitialisation de l_y pour revenir au y d'origine *)

  (* Lignes verticales et les lettres *)
  for i=1 to p_size + 1 do
    if i=(p_size + 1) then (* Si i=p_size + 1, on trace la ligne mais on n'affiche pas de coordonnée *)
      (CPgraphics.moveto(!l_x, !l_y);
        CPgraphics.lineto(!l_x, !l_y + (p_cell_size * p_size)))
    else
      (CPgraphics.moveto(!l_x, !l_y);
        CPgraphics.lineto(!l_x, !l_y + (p_cell_size * p_size));
        CPgraphics.draw_char(char_of_int(64 + i)));
    l_x := !l_x + p_cell_size
  done
;;

(**
Réalise l'affichage des deux grilles du jeu, ainsi que les noms de joueurs.
@param p_cell_size taille des cellules, en pixels.
@param p_grid_size taille des grille, en cellules.
@param p_margin taille de la marge, en pixels.
@param p_message_size hauteur de la zone d'affichage de messages, en pixels.
@author Nadia MOUACHA
@since version 1
*)
let display_empty_grids(p_size, p_cell_size, p_margin, p_message_size : int * int * int * int) : unit =
  let l_x : int ref = ref (p_margin)
  and l_y : int ref = ref (p_margin + p_message_size)
  and grid_px : int = p_size * p_cell_size
  in
  CPgraphics.moveto(0, 0);
  grid_displayer(!l_x, !l_y, p_cell_size, p_size); (* grille ordinateur *)
  l_x := !l_x + p_cell_size + grid_px + p_margin ;
  grid_displayer(!l_x, !l_y, p_cell_size, p_size);(* grille joueur *)
  l_y := !l_y + grid_px + (p_cell_size * 2);
  CPgraphics.moveto(!l_x + p_cell_size, !l_y);
  CPgraphics.draw_string("Ordinateur");
  l_x := p_margin + p_cell_size;
  CPgraphics.moveto(!l_x, !l_y);
  CPgraphics.draw_string("Joueur")
;;

(* ITERATION 2 *)

(**
 Facilite l'indexage de la matrice pour accéder à une cellule.
 @param p_grid_coords coordonnée sur la grille graphique
 @return un couple d'entier. Le second représente le tableau où se trouve la cellule et le premier représente son index dans ce tableau.
 @author Bentz POLO
 @since version 2
 *)
let cell_index(p_grid_coords : char * int) : int * int =
  let l_x : char = fst(p_grid_coords)
  and l_y : int = snd(p_grid_coords)
  in
  (int_of_char(l_x) - int_of_char('A'), l_y - 1)
;;

(**
 Génère une matrice pour représenter une grille.
 @param p_grid_size taille de la grille à représenter
 @return une matrice carrée de taille [p_grid_size]
 @author Bentz POLO
 @since version 2
 *)
let generate_grid_matrix(p_grid_size : int) : t_grid =
  (* On initialise la matrice à la taille nécessaire *)
  let l_grid : t_cell array array = AP1array.mat_make(p_grid_size, p_grid_size, {coord = ('A', 1); state = {contents = EMPTY}})
  in
  (* On parcourt la matrice afin d'initialiser les bonnes coordonnées dans les cellules *)
  for i=0 to Array.length(l_grid) - 1 do
    for j=0 to Array.length(l_grid) - 1 do
      l_grid.(i).(j) <- {coord = (char_of_int(int_of_char('A') + j), i+1); state = {contents = EMPTY}}
    done
  done ;
  l_grid
;;

(**
Place un bateau sur la grille.
@param p_positions cellules que doit bateau à placer.
@p_grid grille dans où il faut placer le bateau.
@author Bentz POLO
@since version 2
*)
let rec place_ship(p_positions, p_grid : (char * int) list * t_grid): unit =
  if List.is_empty(p_positions) then
    ()
  else
    let l_target : (int * int) = cell_index(List.hd(p_positions))
    in
    p_grid.(snd(l_target)).(fst(l_target)).state := OCCUPIED;
    place_ship(List.tl(p_positions), p_grid)
;;

(**
Calcule la liste des positions occupées par un bateau.
@param p_start position de départ (colonne, ligne)
@param p_dir direction du bateau ('l', 'r', 'u', 'd')
@param p_length taille du bateau
@return liste des positions occupées
@author Nadia Mouacha
@since version 2
*)
let rec positions_list (p_start, p_dir, p_length  : (char * int) * t_direction * int) : (char * int) list =
  if p_length <= 0 then
    []
  else
    let l_col : char = fst (p_start)
    and l_row : int = snd (p_start) in

    let l_next_pos : char * int =
      if p_dir = UP then
        (l_col, l_row - 1)
      else 
      if p_dir = DOWN then
        (l_col, l_row + 1)
      else 
      if p_dir = LEFT then
        (char_of_int (int_of_char l_col - 1), l_row)
      else 
      if p_dir = RIGHT then
        (char_of_int (int_of_char l_col + 1), l_row)
      else
        failwith "Direction invalide"
    in
    p_start :: positions_list (l_next_pos, p_dir, (p_length - 1))
;;

(**
 Détermine si le bateau peut etre placé ou pas
 @param p_start position de départ du bateau :colonne et ligne
 @param p_direction direction du bateau "H" = horizontal et "V" =  vertical
 @param p_length longueur du bateau à placer
 @param p_grid grille de jeu dans laquelle le bateau sera placé
 @return true si le bateau peut être placé sans dépasser la grille et si une case ne contient pas déja un bateau, false sinon
 @author Zeinebou NIANG
 @since version 2
 *)
let can_place_ship (p_start, p_direction, p_length, p_grid, p_params : (char * int) * t_direction * int * t_grid * t_params) : bool =
  (* Récupérer la liste des positions du bateau *)
  let l_positions_list = positions_list(p_start, p_direction, p_length) in

  (* Vérifier si toutes les positions sont dans les limites de la grille *)
  let l_within_bounds =
    List.for_all (fun (l_col, l_row) ->
      (* La colonne doit être entre 'A' et la taille de la grille *)
      int_of_char l_col >= int_of_char 'A' && int_of_char l_col < int_of_char 'A' + !(p_params.grid_size) &&
      l_row >= 1 && l_row <= !(p_params.grid_size)
    ) l_positions_list
  in

  if not l_within_bounds then
    false
  else
    (* Vérifier si toutes les positions sont libres sur la grille *)
    let l_positions_free = 
      List.for_all (fun (l_col, l_row) ->
        (* La cellule doit être vide pour pouvoir placer le bateau *)
        p_grid.(l_row - 1).(int_of_char l_col - int_of_char 'A') = { coord = (l_col, l_row); state = {contents = EMPTY}}
      ) l_positions_list
    in
    l_positions_free
;;

(**
Place successivement tous les bateaux à placer dans la grille,
et renvoit une liste de bateaux placés.
@param p_ships liste contenant les bateaux à placer
@param p_grid grille où il faut placer les bateaux.
@return une liste de tout les bateaux placés sur la grille
@author Bentz POLO
@since version 2
*)
let rec auto_placing_ships(p_ships, p_grid, p_param : (string * int) list * t_grid * t_params) : t_ship list =
  if List.is_empty(p_ships) then
    []
  else
    let l_pos_x : char = char_of_int(int_of_char('A') + Random.int(Array.length(p_grid)))
    and l_pos_y : int = Random.int(Array.length(p_grid))
    and l_direction : t_direction = [| UP ; DOWN ; LEFT ; RIGHT |].(Random.int(4))
    in
    let l_current_ship : t_ship = {name = fst(List.hd(p_ships)); positions = positions_list((l_pos_x, l_pos_y), l_direction, snd(List.hd(p_ships)))}
    in
    if can_place_ship(List.hd(l_current_ship.positions), l_direction, snd(List.hd(p_ships)), p_grid, p_param) then
      (place_ship(l_current_ship.positions, p_grid);
        [l_current_ship] @ auto_placing_ships(List.tl(p_ships), p_grid, p_param))
    else
      auto_placing_ships(p_ships, p_grid, p_param)
;;

(**
Transforme les coordonnée d'une cellule en celles du pixel en bas à gauche de cette cellule.
@param p_coords coordonnées de la cellules.
@param init_coords coordonnées initiale de la grille.
@param taille des cellules en pixel.
@param p_grid_size taille de la grille en pixel.
@return les coordonnées du pixel inférieur gauche de la cellule.
@author Bentz POLO
@since version 2
*)
let cell_to_pixel(p_coords, init_coords, p_cell_size, p_grid_size : (char * int) * (int * int) * int * int) : int * int =
  let l_x : int = ((int_of_char(fst(p_coords)) - int_of_char('A')) * p_cell_size)
    + fst(init_coords)
    + p_cell_size (* p_cell_size ajouté pour compense le decalage causé par l'affichage des chiffres *)
  and l_y : int = ((p_grid_size - snd(p_coords)) * p_cell_size) +snd(init_coords)
  in
  (l_x, l_y)
;;


(**
Colore une cellule donnée
@param p_coords coordonnée de la cellule à colorer.
@param p_color couleur à utiliser poue colorer la case.
@param p_params paramètres du jeu.
@param p_player determine dans quelle grille se trouve la cellule à colorier.
@return Rien colore juste la case.
@author Bentz POLO
@since version 2
*)
let color_cell(p_coords, p_color, p_params, p_player : (char * int) * CPgraphics.t_color * t_params * t_where) : unit =
  let (l_x , l_y) : int * int = if p_player = ORDINATEUR then
    cell_to_pixel(p_coords,(!(p_params.margin), !(p_params.margin) + !(p_params.message_size)), !(p_params.cell_size), !(p_params.grid_size))
    else
      let x_offset : int = !(p_params.margin) + !(p_params.grid_size) * !(p_params.cell_size) + !(p_params.cell_size) in
      cell_to_pixel(p_coords, (!(p_params.margin) + x_offset, !(p_params.margin) + !(p_params.message_size)), !(p_params.cell_size), !(p_params.grid_size))
  in
  CPgraphics.set_color(p_color);
  CPgraphics.fill_rect(l_x + 1, l_y + 1, !(p_params.cell_size) - 1, !(p_params.cell_size) - 1)
;;

(* NOTE: List.nth ne fonctionne pas avec les conventions
d'écriture établies en cours*)
(**
Scanne la liste de bateau placés et colores le cases qu'ils occupents
@param p_grid representation matricielle de la grille
@param p_params paramètres du jeu
@author Bentz POLO
@since version 2
*)
(*let display_grid(p_ships, p_grid , p_params, p_player: t_ship list * t_grid * t_params * t_where) : unit =*)
(*  if p_player = ORDINATEUR then*)
(*    ()*)
(*  else*)
(*    for i=0 to List.length(p_ships) - 1 do*)
(*      for j=0 to List.length((List.nth p_ships i).positions) - 1 do*)
(*        color_cell((List.nth (List.nth p_ships i).positions j), CPgraphics.grey, p_params, p_player)*)
(*      done*)
(*    done*)
(*;;*)

(* ITERATION 3 *)
(**

Modifier la fonction display_grid (à l'itération 2)
Affiche toute la grille selon l’état de chaque cellule.
Si le joueur est JOUEUR, les bateaux sont affichés en gris.
Sinon, seuls les effets des tirs sont visibles.
@param p_ships Liste des bateaux (non utilisée ici mais conservée pour compatibilité).
@param p_grid Matrice des cellules du jeu.
@param p_params Paramètres du jeu.
@param p_player JOUEUR ou ORDINATEUR.
@author Nadia MOUACHA
*)
let display_grid (p_grid, p_params, p_player : t_grid * t_params * t_where) : unit =
  for i = 0 to Array.length p_grid - 1 do
    for j = 0 to Array.length p_grid.(i) - 1 do
      let cell = p_grid.(i).(j) in
      let color =
        if !(cell.state) = EMPTY then
          CPgraphics.white
        else if !(cell.state) = OCCUPIED then
          if p_player = JOUEUR then CPgraphics.grey else CPgraphics.white
        else if !(cell.state) = CLICKED then
          CPgraphics.blue
        else if !(cell.state) = TOUCHED then
          CPgraphics.orange
        else if !(cell.state) = DESTROYED then
          CPgraphics.red
        else
          CPgraphics.white  (* Couleur par défaut si l'état est inconnu *)
      in
      color_cell (cell.coord, color, p_params, p_player)
    done
  done
;;


(**
  Retourne la liste des positions voisines (haut, bas, gauche, droite)
  autour de la position [pos], en s'assurant qu'elles restent dans les limites de la grille.

  @param pos La position centrale sous forme de couple (colonne, ligne).
  @return Une liste de positions voisines directement adjacentes dans la grille.
          Les positions retournées sont toujours valides (entre 'A' et 'J', lignes 0 à 9).
  @author Nadia MOUACHA
*)
let get_neighbors (p_pos : char * int) : (char * int) list =
  let (col, row) : char * int = p_pos in
  let l_col_code : int  = int_of_char col in
  let l_neighbors : (char * int) list ref = ref [] in

  (* gauche *)
  if l_col_code > int_of_char 'A' then
    l_neighbors := (char_of_int (l_col_code - 1), row) :: !l_neighbors;

  (* droite *)
  if l_col_code < int_of_char 'A' + 9 then
    l_neighbors := (char_of_int (l_col_code + 1), row) :: !l_neighbors;

  (* haut *)
  if row > 0 then
    l_neighbors := (col, row - 1) :: !l_neighbors;

  (* bas *)
  if row < 9 then
    l_neighbors := (col, row + 1) :: !l_neighbors;

  !l_neighbors
;;

(**
  Fait couler un bateau : met toutes les cellules connectées à celle de départ
  à l'état DESTROYED (coulé) si elles sont à l'état TOUCHED.

  @param p_position Coordonnées de la cellule de départ.
  @param p_grid Grille contenant les cellules.
  @param p_params Paramètres du jeu.
  @author Nadia MOUACHA
*)
let sink_ship (p_position, p_grid, p_params : (char * int) * t_grid * t_params) : unit =
  let rec sink_ship_rec (position : char * int) : unit =
    let (col,row) : char * int = position in
    let (col_index, row_index ) : int * int = cell_index (col, row) in
    let cell : t_cell = p_grid.(row_index).(col_index) in

    if !(cell.state) = TOUCHED then (
      cell.state := DESTROYED;
      CPgraphics.set_color CPgraphics.red;  

    )
    else (
      (* Si la cellule n'est pas touchée on fait rien *)
      ()
    );

    let neighbors : (char * int) list = get_neighbors (col, row) in
    
    for i = 0 to List.length neighbors - 1 do
      let (n_col, n_row ): char * int = List.nth neighbors i in
      sink_ship_rec (n_col, n_row)
    done
  in
  sink_ship_rec (p_position)
;;
(*iteration 3*)

(**
Affiche le message en dessous de la grille 
 @param p_params Structure contenant les paramètres du jeu (la marge, la taille des cellules, la taille de la zone de message, la taille de la grille, et les tailles des bateaux.)
 @param p_message liste des caratctère representent les messages 
 @author Nadia MOUACHA
*)
let display_message (p_message, p_params: string list * t_params) : unit =
  (* Effacer la zone bleue *)
  let l_width : int = 2 * !(p_params.cell_size) * !(p_params.grid_size) + 3 * !(p_params.margin) and l_height : int = !(p_params.message_size) in
  CPgraphics.set_color (CPgraphics.white);
  CPgraphics.fill_rect (0, 0, l_width, l_height);

  (* le texte *)
  CPgraphics.set_color (CPgraphics.black);
  (*CPgraphics.set_text_size (20);*)

  (* Coordonnées de départ *)
  let start_x : int = !(p_params.margin) and  start_y : int = 5 and line_height : int = 25 in


  (* Affichage des messages *)
  for i = 0 to List.length p_message - 1 do
    CPgraphics.moveto (start_x, start_y + i * line_height);
    CPgraphics.draw_string (List.nth p_message i)
  done
;;

(* HACK: Implementation provisoire en attendant celle de Nadia *)
(**
Affiche des messages au joueur.
@param p_message chaine de charactère qu'il faudra afficher au joueur.
@param p_params paramètres du jeu.
@author Bentz POLO
*)
(*let display_message(p_message, p_params : string list * t_params ) : unit =*)
(*  let l_x : int = !(p_params.margin) + 2 * !(p_params.cell_size) * (!(p_params.grid_size) + 2) (* longueur de la zone d'affichage *)*)
(*  and l_y : int = !(p_params.message_size) (* hauteur de la zone d'affichage *)*)
(*  in*)
(*  CPgraphics.moveto(!(p_params.margin), !(p_params.margin));*)
(*  CPgraphics.set_color(CPgraphics.white);*)
(*  CPgraphics.fill_rect(!(p_params.margin), !(p_params.margin), l_x, l_y);*)
(*  (* CPgraphics.draw_rect(!(p_params.margin), !(p_params.margin), l_x, l_y); *)*)
(*  CPgraphics.set_color(CPgraphics.black);*)
(*  for i=0 to List.length(p_message) - 1 do*)
(*    CPgraphics.moveto(!(p_params.margin),(!(p_params.margin) + l_y) - (i+2) * !(p_params.cell_size));*)
(*    CPgraphics.draw_string(List.nth p_message i)*)
(*  done*)
(*;;*)

(**
   Renvoi la cellule où se trouve un pixel donné
   @param p_x coordonné x du pixel en question
   @param p_y coordonné y du pixel en question
   @param p_param paramètres du jeu
   @return Un couple (char, int) qui sont les coordonnées de la cellule cliqué
          renvoi les valeurs ('%', 0) pour un pixel hors grille
   @author Bentz POLO
   *)
let cell_of_pixel(p_x, p_y, p_params : int * int * t_params) : char * int =
  let l_y1 : int = !(p_params.margin) + !(p_params.message_size)
  and l_x1 : int = !(p_params.margin) + !(p_params.cell_size)
  in
  let l_x2 : int = l_x1 + !(p_params.grid_size) * !(p_params.cell_size)
  and l_y2 : int = l_y1 + !(p_params.grid_size) * !(p_params.cell_size) + !(p_params.cell_size)
  in let l_x3 : int = l_x2 + !(p_params.margin)
  in let l_x4 : int = l_x3 + !(p_params.grid_size) * !(p_params.cell_size)
  in
  if l_y1 < p_y && p_y < l_y2 && ((l_x1 < p_x && p_x < l_x2) ||  (l_x3 < p_x && p_x < l_x4)) then
    (char_of_int (p_x / !(p_params.cell_size) + int_of_char('A')), !(p_params.grid_size) - p_y / !(p_params.cell_size))
  else
    ('%', 0)
;;

(**
   Attends un clic de l'utilisateur et renvoi la grille et la cellule cliquée
   @param p_params paramètres du jeu
   @return La grille ainsi que les coordonné de la cellule cliquée
   @author Bentz Polo
   *)
let read_mouse (p_params : t_params) : t_where * (char * int) =
  let (l_px, l_py) : int * int = CPgraphics.wait_button_down()
  and l_y1 : int = !(p_params.margin) + !(p_params.message_size)
  and l_x1 : int = !(p_params.margin) + !(p_params.cell_size)
  in let l_x2 : int = l_x1 + !(p_params.grid_size) * !(p_params.cell_size)
  and l_y2 : int = l_y1 + !(p_params.grid_size) * !(p_params.cell_size) + !(p_params.cell_size)
  in let l_x3 : int = l_x2 + !(p_params.margin)
  in let l_x4 : int = l_x3 + !(p_params.grid_size) * !(p_params.cell_size)
  in
  if l_y1 < l_py && l_py < l_y2 then
    if (l_x1 < l_px && l_px < l_x2) then
      (ORDINATEUR, cell_of_pixel(l_px, l_py, p_params))
    else if (l_x3 < l_px && l_px < l_x4) then
      (JOUEUR, cell_of_pixel(l_px, l_py, p_params))
    else
      (NONE, cell_of_pixel(l_px, l_py, p_params))
  else
    (NONE, cell_of_pixel(l_px, l_py, p_params))
;;

(** Vérifie que 2 cellules sont voisines
 @param p_cell1 coordonnées de la cellule 1 (départ)
 @param p_cell2 coordonnées de la cellule 2 (arrivée)
 @return un tableau de [t_cell]s. Peut renvoyer des voisins "imaginaires".
 @since version 3
 @author Bentz Polo
 *)
let cell_is_neighbour(p_cell1, p_cell2 : (char * int) * (char * int)) : bool =
  let (l_x1, l_y1) : char * int = p_cell1
  and (l_x2, l_y2) : char * int = p_cell2
  in
  (l_x1 = l_x2 && (l_y1 + 1 = l_y2 || l_y1 - 1 = l_y2)) (* voisin d'en haut ou d'en bas *)
  ||
  (l_y1 = l_y2 && (char_of_int(int_of_char(l_x1)+ 1) = l_x2 || char_of_int(int_of_char(l_x1) - 1) = l_x2)) (* voisin de gauche ou de droite *)
;;

(** Determine l'orientation d'une cellule par rapport à une autre 
    @param p_cell1 coordonnée de la cellule 1
    @param p_cell2 coordonnée de la cellule 2
    @return l'orientation de [p_cell2] par rapport à [p_cell1]
    @since version 3
    @author Bentz Polo
    *)
let find_orientation(p_cell1, p_cell2 : (char * int) * (char * int)) : t_direction =
  let (l_x1, l_y1) : char * int = p_cell1
  and (l_x2, l_y2) : char * int = p_cell2
  in
  (* Si le point n'ont pas de coordonné commune cela veut dire qu'elle sont en diagonale *)
  if l_x1 <> l_x2 && l_y1 <> l_y2 then
    failwith "Error: Diagonal orientations not handled."
  else
    (* Si elles ont la meme abscisse, on compare leur ordonnée pour determine l'orientation*)
    if l_x1 = l_x2 then
      if l_y1 > l_y2 then
        UP
      else
        DOWN
    (* Si elles ont pas la meme ordonée, on compare les abscisses *)
    else
    if l_x1 > l_x2 then
      LEFT
    else
      RIGHT
;;

(**
 Permet au joueur de placer ses bateaux sur la grille
 et renvoie la liste des bateaux placé
 @param p_ship_list liste des bateau à placer 
 @param p_grid grille où il faut placer les bateaux
 @param p_params paramètres globaux du jeux
 @since version 3
 @return Une liste contenant les bateaux placés [t_ship]
 @author Bentz Polo
 *)
let rec manual_placing_ships(p_ship_list, p_grid, p_params : (string * int) list * t_grid * t_params) : t_ship list =
  if List.is_empty(p_ship_list) then
    []
  else
    let l_click : (t_where * (char * int)) ref = display_message(["Placez vos bateaux:"; "Veuillez cliquer sur une case de votre grille"], p_params);
    ref(read_mouse(p_params))
  and l_valid_click : bool ref = ref false
  in
  (* Utilise une boucle qui s'arrete quand le joueur clique sur une case de sa grille *)
  let l_cell1 = while not(!l_valid_click) do
    if fst(!l_click) <> JOUEUR then
      (display_message(["!! Veuillez cliquer dans votre grille !!"], p_params);
                    l_click := read_mouse(p_params))
  else
    l_valid_click := true
done ; snd(!l_click)
    in
  (* On colore la case qui vient d'etre cliquée *)
  color_cell(l_cell1, CPgraphics.yellow, p_params, JOUEUR);
  l_valid_click := false ; (* On réinitialise valid_click pour la prochaine boucle *)
  l_click := ( display_message(["Cliquez sur un case adjacente pour terminer"], p_params); 
               read_mouse(p_params) ); (* On attend le prochain click *)
  (* Une boucle qui ne s'arrete pas tant que la case clique n'est pas un voisin de la première *)
  let l_cell2 = while not(!l_valid_click) do
    if fst(!l_click) <> JOUEUR then
      ( display_message(["!! Veuillez cliquer sur une case de votre grille !!"], p_params);
                      l_click := read_mouse(p_params))
    else if not(cell_is_neighbour(l_cell1, snd(!l_click))) then
      ( display_message(["!! Veuillez cliquer sur une case adjacente à la première !!"], p_params);
                      l_click := read_mouse(p_params) )
    else
      l_valid_click := true
  done; snd(!l_click)
  in
  let current_ship : t_ship = { name = fst(List.hd(p_ship_list));
                                positions = positions_list(l_cell1, find_orientation(l_cell1, l_cell2), snd(List.hd(p_ship_list)))}
  in
  if can_place_ship(l_cell1, find_orientation(l_cell1, l_cell2), snd(List.hd(p_ship_list)), p_grid, p_params) then
    ( place_ship(current_ship.positions, p_grid);
      display_grid(p_grid, p_params, JOUEUR);
      [current_ship] @ manual_placing_ships(List.tl(p_ship_list), p_grid, p_params) )
  else
    ( display_message(["Le bateau ne peut pas etre placé ainsi."], p_params);
      display_grid(p_grid, p_params, JOUEUR);
      manual_placing_ships(p_ship_list, p_grid, p_params) )
;;

(*Itération 4*)
(**
  Cherche dans la liste des bateaux si un bateau a été touché à l'endroit où le joueur a cliqué.
  @param p_ships Liste des bateaux
  @param p_grid Grille de jeu
  @param p_params Paramètres du jeu pour lire où le joueur a cliqué
  @return Le bateau touché ou un bateau vide si rien n'est touché
  @author Niang Zeienebou
*)
let find_ship (p_ships, p_grid, p_params : t_ship list * t_grid * t_params) : t_ship =
  (* Crée un bateau vide (au cas où rien n'est touché) *)
  let l_empty_ship : t_ship = {
    name = ""; 
    positions = []; 
  } in

  (* Lis le clic du joueur *)
  let (l_where, (l_clicked_col, l_clicked_row)) = read_mouse(p_params) in

  (* Compte le nombre de bateaux *)
  let l_nb_ships = List.length (p_ships) in

  (* Crée une variable pour stocker le bateau trouvé *)
  let l_found_ship = ref l_empty_ship in

  (* Crée un compteur pour parcourir tous les bateaux *)
  let l_i = ref 0 in

  (* Tant qu'on n'a pas fini de parcourir tous les bateaux *)
  while (!l_i < l_nb_ships) do
    (* Prend le i-ème bateau *)
    let l_current_ship = List.nth p_ships (!l_i) in

    (* Compte combien de cases il occupe *)
    let l_nb_positions = List.length (l_current_ship.positions) in

    (* Crée un compteur pour parcourir toutes ses cases *)
    let l_j = ref 0 in

    (* Tant qu'on n'a pas fini de vérifier toutes les cases du bateau *)
    while (!l_j < l_nb_positions) do
      (* Prend la j-ème position du bateau *)
      let (l_ship_col, l_ship_row) = List.nth (l_current_ship.positions) (!l_j) in

      (* Vérifie si cette position est la même que celle cliquée *)
      if (l_ship_col = l_clicked_col && l_ship_row = l_clicked_row) then
        (* Prend la cellule correspondante dans la grille *)
        let l_cell = p_grid.(l_ship_row - 1).(int_of_char (l_ship_col) - int_of_char ('A')) in

        (* Vérifie si cette cellule a l'état TOUCHED *)
        if (!(l_cell.state) = TOUCHED) then
          (* Si oui on a trouvé le bateau *)
          l_found_ship := l_current_ship;
        ;

      (* Passe à la prochaine case du bateau *)
      l_j := !l_j + 1
    done;

    (* Passe au prochain bateau *)
    l_i := !l_i + 1
  done;

  (* Retourner le bateau trouvé, ou un bateau vide si rien trouvé *)
  !l_found_ship
;;

(**
  Permet au joueur de tirer sur la grille de l'ordinateur.
  Si le joueur clique en dehors ou sur une mauvaise grille, il doit recommencer.
  Si clic sur un bateau : état TOUCHED et couleur rouge.
  Si clic sur vide : état CLICKED et couleur verte.
  @param p_grid la grille de l'ordinateur
  @param p_params les paramètres du jeu
  @author Niang Zeinebou
  @return unit
*)
let rec player_shoot (p_grid , p_params : t_grid * t_params) : unit =
  (* Lis où le joueur clique *)
  let (l_player, l_coords) = read_mouse(p_params)
  in

  (* Vérifie si le clic est sur la grille de l'ordinateur *)
  if (l_player = ORDINATEUR) then
    (* Récupère la colonne et la ligne *)
    let l_col = fst(l_coords)
    and l_row = snd(l_coords)
    in

    (* Vérifie si les coordonnées sont valides *)
    if (l_col <> '%') then
      (* Récupère la cellule correspondante dans la grille *)
      let l_cell = p_grid.(l_row - 1).(int_of_char(l_col) - int_of_char('A'))
      in

      (* Vérifie si la cellule contient un bateau *)
      if (!(l_cell.state) = OCCUPIED) then
        (* Marque la cellule comme touchée et colore en rouge *)
        (l_cell.state := TOUCHED;
         color_cell((l_col, l_row), CPgraphics.orange, p_params, ORDINATEUR))
      else
        (* Marque la cellule comme cliquée et colorie en vert vu qu'elle ne contient pas de bateau *)
        (l_cell.state := CLICKED;
         color_cell((l_col, l_row), CPgraphics.green, p_params, ORDINATEUR))
    else
      (* les coordonnées sont invalides le joueur recommence *)
      player_shoot(p_grid,p_params)
  else
    (* le joueur n'a pas cliqué sur la bonne grille et doit recommencer *)
    player_shoot(p_grid,p_params)
;;

(*Itération 5*) 
(**
  Permet à l'ordinateur de tirer sur la grille du joueur.
  Utilise cell_to_pixel et color_cell pour gérer l'affichage.
  @param p_player_grid La grille du joueur
  @param p_params Les paramètres du jeu
  @author Niang Zeinebou
*)
let computer_shoot (p_player_grid, p_params : t_grid * t_params) : unit =
  (* Variables pour stocker la position du tir *)
  let l_col = ref 'A' in
  let l_row = ref 1 in

  (* Variable pour vérifier si le tir est valide *)
  let l_valid_shot = ref false in

  (* Boucle pour choisir une case non déjà touchée ou cliquée *)
  while (!l_valid_shot = false) do
    (* Choisi une colonne entre 'A' et 'J' *)
    l_col := char_of_int (int_of_char 'A' + Random.int (10));

    (* Choisi une ligne entre 1 et 10 *)
    l_row := 1 + Random.int (10);

    (* Récupére la cellule correspondante *)
    let l_cell = p_player_grid.(!l_row - 1).(int_of_char (!l_col) - int_of_char ('A')) in

    (* Vérifie que la cellule n'a pas encore été jouée *)
    if (!(l_cell.state) <> TOUCHED && !(l_cell.state) <> CLICKED) then
      l_valid_shot := true
  done;

  (* Une cellule valide a été trouvée *)

  (* Récupére la cellule choisie *)
  let l_final_cell = p_player_grid.(!l_row - 1).(int_of_char (!l_col) - int_of_char ('A')) in

  (* Coordonnées  de la cellule *)
  let l_coord = (!l_col, !l_row) in

  (* Si la cellule contient un bateau *)
  if (!(l_final_cell.state) = OCCUPIED) then
    (
      (* Marquer la cellule comme touchée *)
      l_final_cell.state := TOUCHED;

      (* Colorie la cellule en rouge *)
      color_cell(l_coord, CPgraphics.red, p_params, JOUEUR)
    )
  else
    (
      (* Sinon marquer la cellule comme cliquée *)
      l_final_cell.state := CLICKED;

      (* Colorie la cellule en vert *)
      color_cell(l_coord, CPgraphics.green, p_params, JOUEUR)
    )
;;


(**
   Prend les paramètres du jeu, place les bateaux de l'ordi et permet au joueur de placer ses bateaux
   @param p_params paramètres du jeu
   @return Un [t_battleship] qui contient les grilles et les liste des bateaux du joueur et de l'ordi
   @author Bentz Polo
 *)
let init_battleship(p_params : t_params) : t_battleship =
  let l_player_grid = generate_grid_matrix(!(p_params.grid_size))
  and l_computer_grid = generate_grid_matrix(!(p_params.grid_size))
  in
  let l_player_ships = manual_placing_ships(!(p_params.ship_sizes), l_player_grid, p_params)
  and l_computer_ships = auto_placing_ships(!(p_params.ship_sizes), l_computer_grid, p_params)
  in
  let battleship : t_battleship ={
    player_grid = l_player_grid; 
    computer_grid = l_computer_grid;
    player_ships = l_player_ships;
    computer_ships = l_computer_ships;
  }
  in battleship
;;

(**
  Fonction principale du jeu. Lance la fenetre graphique et mets à jour le titre et
  enclenche le déroulement du jeu.
  @author Zeinebou NIANG
  @author Bentz POLO
  @since version 1
*)
let battleship_game() : unit =
  let settings : t_params = {
    margin = {contents = 0};
    cell_size = {contents = 0};
    message_size = {contents = 0};
    grid_size = {contents = 0};
    ship_sizes = {contents = []};
  }
  in
  init_params(30, 15, 60, 10,
    [("Porte-avions", 5); ("Croiseur", 4); ("Contre-torpilleur", 3); ("Contre-torpilleur", 3); ("Torpilleur", 2)], settings);
  Random.self_init();
  CPgraphics.open_graph(410,290);
  CPgraphics.set_window_title("Battleship Game");
  display_empty_grids(!(settings.grid_size), !(settings.cell_size), !(settings.margin), !(settings.message_size));
  let play_state = init_battleship(settings)
  in
  color_cell(('D', 4), CPgraphics.yellow, settings, JOUEUR);
  display_grid(play_state.player_grid, settings, JOUEUR);
  display_grid(play_state.player_grid, settings, ORDINATEUR);
  CPgraphics.wait(600)
;;

(* Appel de la fonction principale pour lancer le jeu *)
battleship_game();;
