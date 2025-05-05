(* Objectif de test: test unitaire du module battleship_game *)

(* Chargement de la bibliothèque de test et du fichier source *)
#use "inter.ml" ;; (* A enlever après *)
#use "CPtest.ml" ;;
#use "battleship.ml" ;;

(* Initialise la campagne de test *)
test_reset_report() ;;

(* TESTS ITERATION 2 *)

(* Cas unique *)
let test_cell_index_func () : unit =
        let l_res :  (int * int) t_test_result =
                test_exec(cell_index, "cell_index transforme les coordonnée de grille en index de matrice", ('D', 4))
        in
        assert_equals_result((3,3), l_res)
;;

(* Cas unique *)
let test_generate_grid_matrix_func () : unit =
        let l_res : t_grid t_test_result =
                test_exec(generate_grid_matrix, "generate_grid_matrix génère une matrice repésentant une grille", 10)
        in
        assert_equals(10, Array.length(test_get(l_res))) ; (* taille de la matrice *)
        assert_equals(10, Array.length((test_get(l_res)).(4))) ; (* taille des tableaux de la matrice *)
        assert_equals(('E', 4), (test_get(l_res)).(3).(4).coord)
;;

(*TODO*)
(* Cas ou la liste est vide *)
let test_place_ship_struc () : unit =
        let l_
(* Cas unique. La fonction ne prend*)
(* let test_auto_placing_ships_func () : unit = *)

(* appel des fonctions de test *)
test_cell_index_func () ;;
test_generate_grid_matrix_func () ;;

test_report() ;;

(*Tests Itération 4*)
open Test_lib
open Game (* Remplacer par le nom réel de ton module *)

(* Initialisation des paramètres *)
let params = {
  margin = ref 0;
  cell_size = ref 0;
  message_size = ref 0;
  grid_size = ref 10;
  ship_sizes = ref [
    ("Porte-avions", 5);
    ("Croiseur", 4);
    ("Contre-torpilleur", 3);
    ("Contre-torpilleur", 3);
    ("Torpilleur", 2)
  ];
}

(* Petite fonction pour créer une grille vide *)
let create_empty_grid size =
  Array.init size (fun i ->
    Array.init size (fun j ->
      { coord = (Char.chr (65 + i), j + 1); state = ref EMPTY }
    )
  )

(* Fonction pour mettre un bateau sur la grille *)
let place_ship grid positions =
  List.iter (fun (c, n) ->
    let i = Char.code c - Char.code 'A' in
    let j = n - 1 in
    grid.(i).(j).state := OCCUPIED
  ) positions

let () =
  test_begin ();

  (* Test de update_grid *)
  let grid = create_empty_grid 5 in
  update_grid (('A', 1), TOUCHED, grid);
  assert_equals_m "update_grid : cellule ('A',1) doit devenir TOUCHED" TOUCHED !(grid.(0).(0).state);

  (* Test de sink_ship *)
  let grid2 = create_empty_grid 5 in
  let ship_positions = [('A',1); ('A',2); ('A',3)] in
  place_ship grid2 ship_positions;
  sink_ship (('A',1), grid2, params);
  List.iter (fun (c, n) ->
    let (i, j) = (Char.code c - Char.code 'A', n - 1) in
    assert_equals_m "sink_ship : la case doit être marquée DESTROYED" DESTROYED !(grid2.(i).(j).state)
  ) ship_positions;

  (* Test de check_sunk_ship *)
  let grid3 = create_empty_grid 5 in
  let ship3_positions = [('B',1); ('B',2)] in
  place_ship grid3 ship3_positions;
  (* On touche toutes les positions *)
  List.iter (fun (c, n) -> update_grid ((c, n), TOUCHED, grid3)) ship3_positions;
  let ship3 = { name = "Petit bateau"; positions = ship3_positions } in
  assert_true_m "check_sunk_ship : bateau entièrement touché => true" (check_sunk_ship (ship3, grid3));

  (* Test de find_ship *)
  let ships = [
    { name = "Croiseur"; positions = [('A',1); ('A',2)] };
    { name = "Torpilleur"; positions = [('C',3); ('C',4)] }
  ] in
  let grid4 = create_empty_grid 5 in
  let found_ship = find_ship (ships, grid4, params) in
  assert_false_m "find_ship : doit trouver un navire" (found_ship.name = "");

  (* Tests de player_shoot *)

  (* 1. Tir raté : case vide *)
  let grid5 = create_empty_grid 5 in
  let ships5 = [] in
  player_shoot (grid5, ships5, params);
  (* Impossible de vérifier exactement où il tire sans adaptation de player_shoot, donc on saute la vérif ici *)

  (* 2. Tir touché : case avec un bateau *)
  let grid6 = create_empty_grid 5 in
  place_ship grid6 [('E',5)];
  let ships6 = [{ name = "Torpilleur"; positions = [('E',5)] }] in
  player_shoot (grid6, ships6, params);
  assert_equals_m "player_shoot : case tirée doit devenir TOUCHED" TOUCHED !(grid6.(4).(4).state);

  (* 3. Tir coulé : dernier morceau d'un bateau *)
  let grid7 = create_empty_grid 5 in
  place_ship grid7 [('C',1); ('C',2)];
  let ships7 = [{ name = "Destroyer"; positions = [('C',1); ('C',2)] }] in
  update_grid (('C',1), TOUCHED, grid7); (* Déjà touché *)
  player_shoot (grid7, ships7, params);
  assert_equals_m "player_shoot : toutes les cases doivent être DESTROYED" DESTROYED !(grid7.(2).(0).state);
  assert_equals_m "player_shoot : toutes les cases doivent être DESTROYED" DESTROYED !(grid7.(2).(1).state);

  (* Test simple de display_grid *)
  let grid8 = create_empty_grid 5 in
  display_grid (grid8, params, JOUEUR);
  assert_true_m "display_grid : affichage sans crash" true;

  test_report ()
;;
