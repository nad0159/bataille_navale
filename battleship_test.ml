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

(* ========================================= *)
(* Définition des paramètres pour les tests *)
(* ========================================= *)

let settings : t_params = {
  margin = ref 50;
  cell_size = ref 30;
  message_size = ref 50;
  grid_size = ref 10;
  ship_sizes = ref [("Patrol", 2); ("Destroyer", 3); ("Cruiser", 3)];
}

(* ========================================= *)
(* Outils pour les assertions *)
(* ========================================= *)

let assert_equals (expected : 'a) (actual : 'a) : unit =
  if expected <> actual then
    failwith "Assertion failed"
;;

(* ========================================= *)
(* Tests de update_grid *)
(* ========================================= *)

let test_update_grid () : unit =
  let l_grid = [|
    [| {coord = ('A', 1); state = ref EMPTY} |]
  |] in
  update_grid(('A', 1), OCCUPIED, l_grid);
  assert_equals OCCUPIED !(l_grid.(0).(0).state)
;;

(* ========================================= *)
(* Tests de check_sunk_ship *)
(* ========================================= *)

let test_check_sunk_ship_true () : unit =
  let l_grid = [|
    [| {coord = ('A', 1); state = ref DESTROYED} |]
  |] in
  let l_ship = {name = "TestShip"; positions = [('A', 1)]} in
  assert_equals true (check_sunk_ship (l_ship, l_grid))
;;

let test_check_sunk_ship_false () : unit =
  let l_grid = [|
    [| {coord = ('A', 1); state = ref OCCUPIED} |]
  |] in
  let l_ship = {name = "TestShip"; positions = [('A', 1)]} in
  assert_equals false (check_sunk_ship (l_ship, l_grid))
;;

(* ========================================= *)
(* Tests de find_ship *)
(* ========================================= *)

let test_find_ship () : unit =
  let l_ships = [{name = "Patrol"; positions = [('B', 2)]}] in
  let l_grid = [|
    [| {coord = ('A', 1); state = ref EMPTY};
       {coord = ('B', 1); state = ref EMPTY} |];
    [| {coord = ('A', 2); state = ref EMPTY};
       {coord = ('B', 2); state = ref TOUCHED} |]
  |] in

  let simulated_read_mouse (_ : t_params) : t_where * (char * int) =
    (ORDINATEUR, ('B', 2))
  in

  let original_read_mouse = !read_mouse in
  read_mouse := simulated_read_mouse;

  let l_found_ship = find_ship (l_ships, l_grid, settings) in
  assert_equals "Patrol" l_found_ship.name;

  read_mouse := original_read_mouse
;;

(* ========================================= *)
(* Test de sink_ship *)
(* ========================================= *)

let test_sink_ship_func () : unit =
  let l_test_grid = [|
    [| {coord = ('A', 1); state = ref TOUCHED};
       {coord = ('B', 1); state = ref TOUCHED};
       {coord = ('C', 1); state = ref EMPTY} |]
  |] in

  sink_ship(('A', 1), l_test_grid, settings);

  assert_equals DESTROYED !(l_test_grid.(0).(0).state);
  assert_equals DESTROYED !(l_test_grid.(0).(1).state);
  assert_equals EMPTY !(l_test_grid.(0).(2).state)
;;

(* ========================================= *)
(* Tests de player_shoot *)
(* ========================================= *)

let test_player_shoot_missed () : unit =
  let l_test_grid = [|
    [| {coord = ('A', 1); state = ref EMPTY} |]
  |] in
  let l_test_ships = [] in

  let simulated_read_mouse (_ : t_params) : t_where * (char * int) =
    (ORDINATEUR, ('A', 1))
  in

  let original_read_mouse = !read_mouse in
  read_mouse := simulated_read_mouse;

  player_shoot(l_test_grid, l_test_ships, settings);

  assert_equals CLICKED !(l_test_grid.(0).(0).state);

  read_mouse := original_read_mouse
;;

let test_player_shoot_touched () : unit =
  let l_test_grid = [|
    [| {coord = ('A', 1); state = ref OCCUPIED} |]
  |] in
  let l_test_ships = [
    { name = "Patrol"; positions = [('A', 1)] }
  ] in

  let simulated_read_mouse (_ : t_params) : t_where * (char * int) =
    (ORDINATEUR, ('A', 1))
  in

  let original_read_mouse = !read_mouse in
  read_mouse := simulated_read_mouse;

  player_shoot(l_test_grid, l_test_ships, settings);

  assert_equals TOUCHED !(l_test_grid.(0).(0).state);

  read_mouse := original_read_mouse
;;

let test_player_shoot_sunk () : unit =
  let l_test_grid = [|
    [| {coord = ('A', 1); state = ref OCCUPIED} |]
  |] in
  let l_test_ships = [
    { name = "Patrol"; positions = [('A', 1)] }
  ] in

  let simulated_read_mouse (_ : t_params) : t_where * (char * int) =
    (ORDINATEUR, ('A', 1))
  in

  let original_read_mouse = !read_mouse in
  read_mouse := simulated_read_mouse;

  player_shoot(l_test_grid, l_test_ships, settings);

  assert_equals DESTROYED !(l_test_grid.(0).(0).state);

  read_mouse := original_read_mouse
;;

(* ========================================= *)
(* Lancer tous les tests *)
(* ========================================= *)

let () =
  test_update_grid ();
  test_check_sunk_ship_true ();
  test_check_sunk_ship_false ();
  test_find_ship ();
  test_sink_ship_func ();
  test_player_shoot_missed ();
  test_player_shoot_touched ();
  test_player_shoot_sunk ();
  print_endline " Tous les tests sont passés !"
;;



