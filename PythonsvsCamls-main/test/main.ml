(** CS 3110 FINAL PROJECT Nila Narayan (nn257), Leina Li (ll736),
    Adeniyi Fagbewesa (amf349)*)

open OUnit2
open Src.Characters
open Src.State
open Src.Tile

(* describing your approach to testing: Our approach was to test
   functions that were not demonstratable through the GUI but still
   crucial to the running of the game. Before fully implementing the
   GUI, we tested state functions through the terminal based version of
   the game as well as through our own black box and glass box testing.
   This meant that we could ensure the basic functionality of the game
   before advancing it. Our approach to testing GUI was to see if the
   window had features that we wanted to have on our windows after
   coding. This includes checking if print statements, scores,
   handle_collisions, were all demonstrating what they should be doing.
   For example, we'll randomly watch a character on the board and see
   whether they disappear off the board and if scores update after an
   appropriate constant time of interaction between to characters.*)
(* what you tested: We primarily tested the characters.ml file, because
   certain aspects of an individual character (such as health) can not
   directly be accessed by playing either version of the game. So, we
   tested almost every character function that was not just printing for
   the sake of the GUI. We took a similar approach to state.ml, since it
   serves as the backbone of our entire game. We needed to test that the
   levels could correctly increment. We used both glass box and black
   box testing to develop tests. For glass box testing, we created a
   make bisect function that helps detect what functions were tested. We
   also tried path testing for some of the tests to see if functions
   would properly update states in an orderly fashion. For black box
   texting, we read the mli and tried to develop a test for functions.
   When parts of the mli felt vague during blackbox testing, we looked
   inside our code and tried to update the mli to better represent the
   functions functionality.*)
(* anything you omitted testing: We ommited testing print functions as
   they were hard coded and do not contribute largely to the GUI system
   of the game. Also ommitted testing tile.ml since most of it was
   demonstrated in the GUI. We also ommitted testing functions that
   called tile functions in state.ml since hardcoding a bunch of tiles
   makes it more likely for the correct output to be wrong. So
   demonstrating through GUI was a more effective option since GUI makes
   it easier to see how tiles is handled. An example of a function we
   ommitted is State.handle_collsions for the reasons described above.
   Essentially, any function which we ommitted from testing was directly
   tested by our GUI regardless, both through the terminal version of
   the game and the graphical version. The entire graphical suite tests
   gui.ml, which is why we did not write o-unit tests for it. *)
(* why you believe that your test suite demonstrates the correctness of
   your system: Our test suite proves that the most fundamental
   functionality of the game works. Solidifying these test cases before
   implementing the GUI and terminal based version of the game, allowed
   us to implement visual operations without having to worry about
   breaking the game. This was the main perk of test driven development.
   We know that characters can be successfully spawned and modified (ex.
   remove health), and that actions such as adding and removing objects
   from the board are successful. This meant that, going into our GUI
   development phase, we could be decently confident in our previous
   code. Ultimately, we know our test suite and code as a whole is
   successful because the game works in both terminal and in a graphics
   terminal. *)
let string_of_tuple (x, y) =
  "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let current_location_test
    (name : string)
    (o : objects_state)
    (c : game_objects)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_obj_position c)
    ~printer:string_of_tuple

let get_score_test
    (name : string)
    (o : objects_state)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_score o) ~printer:string_of_int

let get_state_level_test
    (name : string)
    (o : objects_state)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_state_level o)
    ~printer:string_of_int

let get_raindrops_test
    (name : string)
    (o : objects_state)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_raindrops o) ~printer:string_of_int

let get_state_level_test
    (name : string)
    (o : objects_state)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_state_level o)
    ~printer:string_of_int

let state_life_test
    (name : string)
    (o : objects_state)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (state_life o) ~printer:string_of_int

let lose_test
    (name : string)
    (alive : objects_state)
    (dead : objects_state)
    (kill : int) : test =
  name >:: fun _ ->
  assert_raises (Lose dead) (fun () -> lose_life alive kill)
(*NEED REVISION: currently not catching exception still*)

let get_tiles_test
    (name : string)
    (o : objects_state)
    (expected_output : tile list list) : test =
  name >:: fun _ -> assert_equal expected_output (get_tiles o)

let state_add_points_test
    (name : string)
    (o : objects_state)
    (score_to_add : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_score (state_add_points score_to_add o))
    ~printer:string_of_int

let buy_object_test
    (name : string)
    (o : objects_state)
    (obj : game_objects)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_raindrops (buy_object obj o))
    ~printer:string_of_int

let move_spit_state_test
    (name : string)
    (o : objects_state)
    (expected_output : tile list list) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_tiles (move_spit_state o))

let move_python_state_test
    (name : string)
    (o : objects_state)
    (expected_output : tile list list) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_tiles (move_python_state o))

let add_object_test_int
    (name : string)
    (o : objects_state)
    (obj : game_objects)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.length (get_objects (add_object obj o)))

let add_object_test_list
    (name : string)
    (o : objects_state)
    (obj : game_objects)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.map
       (fun g -> object_string g)
       (get_objects (add_object obj o)))

let remove_object_test_int
    (name : string)
    (o : objects_state)
    (obj : game_objects list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.length (get_objects (remove_object (List.nth obj 0) o)))

let remove_object_test_list
    (name : string)
    (o : objects_state)
    (obj : game_objects list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.map
       (fun g -> object_string g)
       (get_objects (remove_object (List.nth obj 0) o)))
(*for ease, just remove the first element from the list*)

let check_state_win_test
    (name : string)
    (o : objects_state)
    (expected_output : objects_state) : test =
  name >:: fun _ -> assert_equal expected_output (check_state_win o)

let check_state_win_exc_test (name : string) o : test =
  name >:: fun _ -> assert_raises (Win o) (fun () -> check_state_win o)

let cactus_rain_state_test
    (name : string)
    (o : objects_state)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_raindrops (cactus_rain_state o))

let initial_state = init_state 0 0
let py_state = init_state ~num_py:10 0 0
let initial_state1 = init_state 1 1
let initial_state2 = init_state 1 2
let lvl1 = next_state_level initial_state
let raindrop_st1 = add_raindrops 10 initial_state1
let raindrop_st2 = add_raindrops 100 initial_state1
let raindrop_st3 = add_raindrops 100 raindrop_st2
let ch_st1 = add_object (init_caml 1 1 1) initial_state1
let spit_st1 = add_object (init_spit 1 1) initial_state2
let py_st1 = add_object (init_python 1 2) initial_state2
let buy_st_test = add_object (init_caml 1 1 1) raindrop_st2
let life_st1 = lose_life initial_state 1 (*2 <3 remains*)

let life_st2 = lose_life initial_state 2 (*1 <3 remains*)

let remove_st_test = add_object (init_caml 1 1 1) initial_state1

let state_tests =
  [
    get_score_test "get_score initial_state is 0" initial_state 0;
    get_state_level_test "get_level intial_state is 0" initial_state 0;
    get_raindrops_test "get_raindrops initial_state is 0" initial_state
      0;
    get_state_level_test "get_state_level initial_state is 0"
      initial_state 0;
    state_life_test "state_life initial_state is 3" initial_state 3;
    (*End of test for basic probing commands for objects_state fields*)
    get_raindrops_test "get_raindrops raindrop_st1 is 10" raindrop_st1
      10;
    get_raindrops_test "get_raindrops raindrop_st2 is 100" raindrop_st2
      100;
    get_raindrops_test "get_raindrops raindrop_st3 is 100" raindrop_st3
      200;
    get_tiles_test "get_tiles initial_state is [[]]" initial_state
      [ [] ];
    get_tiles_test "get_tiles ch_st1 is [[Caml]]" ch_st1
      [ [ make_tile (1, 1) false [ init_caml 1 1 1 ] ] ];
    (*test for adding raindrop *)
    move_spit_state_test "move_spit_state of initial_state is [[]]"
      initial_state [ [] ];
    move_spit_state_test "move_spit_state of spit_st1 is [[],[*]]"
      spit_st1
      [
        [
          make_tile (1, 1) false [];
          make_tile (1, 2) false [ init_spit 1 2 ];
        ];
      ];
    move_python_state_test "move_python_state of initial_state is [[]]"
      initial_state [ [] ];
    move_python_state_test "move_python_state of py_st1 is [[Py],[]]"
      py_st1
      [
        [
          make_tile (1, 1) false [ init_python 1 1 ];
          make_tile (1, 2) false [];
        ];
      ];
    state_life_test "get_life life_st1 is 2" life_st1 2;
    state_life_test "get_life life_st2 is 1" life_st2 1;
    (*End of test for removing lives; NOT INCLUDING EXCEPTION TESTS*)
    (*test for adding points*)
    state_add_points_test "Add 1 point to initial score of 0"
      initial_state 1 1;
    (*test for buying obtject*)
    buy_object_test "Buy a camel, going from 100 to 0 raindrops"
      raindrop_st2 (init_caml 1 1 1) 0;
    get_state_level_test "state_level lvl1 is 1; tests next_state_level"
      lvl1 1;
    get_raindrops_test "get_raindrops lvl1 is 250" lvl1 250
    (*next_state_level tests; raindrops and level should change*);
    check_state_win_exc_test "check_state_win initial_state raises Win"
      initial_state;
    check_state_win_test "check_state_win initial_state raises Win"
      py_state py_state;
    (*tests of adding and removing objects*)
    add_object_test_int
      "add an object to the initial game state, bringing the number of \
       objects from 0 to 1"
      initial_state1 (init_caml 1 1 1) 1;
    add_object_test_list
      "adding an object to the initial empty game state means the list \
       of game objects goes from empty to containing one object, a \
       camel"
      initial_state1 (init_caml 1 1 1) [ "Caml" ];
    remove_object_test_int
      "removing an object from a game state containing one camel \
       results in a came with 0 game objects"
      remove_st_test
      (get_objects remove_st_test)
      0;
    remove_object_test_list
      "removing an object from a game state containing one camel \
       results in a came with 0 game objects, an empty list"
      remove_st_test
      (get_objects remove_st_test)
      [];
  ]

(********************CHARACTERS TESTS**************************)

let are_enemies_test
    (name : string)
    (o1 : game_objects)
    (o2 : game_objects)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (are_enemies o1 o2)
    ~printer:string_of_bool

let compare_game_objects_test1
    (name : string)
    ~(ignore_health : bool)
    (o1 : game_objects)
    (o2 : game_objects)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (compare_game_objects ~ignore_health o1 o2)
    ~printer:string_of_bool

let compare_game_objects_test2
    (name : string)
    (o1 : game_objects)
    (o2 : game_objects)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (compare_game_objects o1 o2)
    ~printer:string_of_bool
(*same thing as [compare_game_objects_test1] but doesn't have optional
  parameter*)

let can_spit_test
    (name : string)
    (o : game_objects)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (can_spit o) ~printer:string_of_bool

let get_health_test
    (name : string)
    (o : game_objects)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_health o) ~printer:string_of_int

let get_rain_rate_test
    (name : string)
    (o : game_objects)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_rain_rate o) ~printer:string_of_int

let get_object_points_test
    (name : string)
    (o : game_objects)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_object_points o)
    ~printer:string_of_int

let get_object_damage_test
    (name : string)
    (o : game_objects)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_object_damage o)
    ~printer:string_of_int

let get_cost_test
    (name : string)
    (o : game_objects)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_cost o) ~printer:string_of_int

let get_obj_position_test
    (name : string)
    (o : game_objects)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_obj_position o)
    ~printer:string_of_tuple

let get_timer_test
    (name : string)
    (o : game_objects)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_timer o) ~printer:string_of_int

let get_char (o : game_objects option) =
  match o with
  | Some o -> o
  | None -> failwith "None"

let caml_e0 = init_caml 0 0 0
let caml_e1 = init_caml ~-1 ~-2 ~-3
let caml_1 = init_caml 1 1 1
let caml_2 = init_caml 2 2 2
let caml_5_10 = init_caml 1 10 5
let caml_10_6 = init_caml 1 10 6 (*y pos out of bound*)

let caml_helth = get_char (damage_character caml_e0 0)
(*should have same health =1 *)

let pos_caml = update_pos (1, 1) caml_e0
let caml_11_5 = init_caml 1 11 5 (*x pos out of bound*)

let caml_max = init_caml max_int max_int max_int (*max everything!*)

let caml_h01 = init_caml 1 1 1 ~health:0 (*caml_1 but health dead*)

let caml_h1 = damage_character caml_1 1 (*caml_1 but dead*)

let cactus_0 = init_cactus 0 0
let cactus_1 = init_cactus 1 1
let cactus_00 = init_cactus ~health:0 0 0 (*cactus_0 but health dead*)

let pos_cactus = update_pos (3, 3) cactus_0
let cactus_000 = damage_character cactus_0 10 (*cactus_0 but dead*)

let cactus_helth = get_char (damage_character cactus_0 5)
let python_0 = init_python 0 0
let python_00 = init_python 0 0 ~health:0 (*python_0 but health dead*)

let python_helth = get_char (damage_character python_0 1)
(*python_0 now has health = 1*)

let python_1 = init_python 1 1
let pos_python = update_pos (max_int, max_int) python_0
let spit_0 = init_spit 0 0
let spit_1 = init_spit 1 1
let pos_spit = update_pos (0, 0) spit_0
let pos1_spit = update_pos (3, 3) pos_spit
let pos2_spit = update_pos (-10, -10) pos1_spit
let caml_incr = incr_spit_timer caml_1
let caml_incr2 = incr_spit_timer caml_incr
let caml_incr3 = incr_spit_timer caml_incr2
let py_incr = incr_spit_timer python_0

let characters_test =
  [
    are_enemies_test "are_enemies caml_1 python_1 is true" caml_1
      python_1 true;
    are_enemies_test "are_enemies python_1 caml_1 is true" python_1
      caml_1 true;
    are_enemies_test "are_enemies caml_0 python_0 is true" caml_e0
      python_0 true;
    are_enemies_test "are_enemies python_1 spit_1 is true" python_1
      spit_1 true;
    are_enemies_test "are_enemies spit_1 python_1 is true" spit_1
      python_1 true;
    are_enemies_test "are_enemies python_1 cactus_1 is true" python_1
      cactus_1 true;
    are_enemies_test "are_enemies cactus_1 python_1 is true" cactus_1
      python_1 true;
    are_enemies_test "are_enemies caml_1 caml_2 is false" caml_1 caml_2
      false;
    are_enemies_test "are_enemies caml_2 caml_1 is false" caml_2 caml_1
      false;
    are_enemies_test "are_enemies cactus_0 cactus_1 is false" cactus_0
      cactus_1 false;
    are_enemies_test "are_enemies cactus_1 cactus_0 is false" cactus_1
      cactus_0 false;
    are_enemies_test "are_enemies spit_0 spit_1 is false" spit_0 spit_1
      false;
    are_enemies_test "are_enemies python_1 python_0 is false" python_1
      python_0 false;
    are_enemies_test "are_enemies python_0 python_1 is false" python_0
      python_1 false
    (* End of are_enemies_test, missing incr_spit_timer, update_pos *);
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true caml_1 caml_2 is false"
      ~ignore_health:true caml_1 caml_2 false;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false caml_1 caml_2 is false"
      ~ignore_health:false caml_1 caml_2 false;
    compare_game_objects_test2
      "compare_game_objects caml_1 caml_2 is false" caml_1 caml_2 false;
    (* Comparing different objects of same type Caml *)
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true python_0 python_1 is \
       false"
      ~ignore_health:true python_0 python_1 false;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false python_0 python_1 is \
       false"
      ~ignore_health:false python_0 python_1 false;
    compare_game_objects_test2
      "compare_game_objects python_0 python_1 is false" python_0
      python_1 false;
    (* Comparing different objects of same type Python*)
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false python_0 python_0 is \
       true"
      ~ignore_health:false python_0 python_0 true;
    compare_game_objects_test2
      "compare_game_objects python_1 python_1 is false" python_1
      python_1 true;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false python_0 python_00 is \
       false"
      ~ignore_health:false python_0 python_00 false;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true python_0 python_00 is \
       true"
      ~ignore_health:true python_0 python_00 true;
    (* Comparing same objects of type Python*)
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true caml_1 caml_1 is true"
      ~ignore_health:true caml_1 caml_1 true;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false caml_1 caml_1 is true"
      ~ignore_health:false caml_1 caml_1 true;
    compare_game_objects_test2
      "compare_game_objects caml_1 caml_1 is true" caml_1 caml_1 true;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false caml_1 caml_1 is true"
      ~ignore_health:false caml_1 caml_1 true;
    (*Comparing the same camls*)
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true caml_1 caml_h1 is true"
      ~ignore_health:true caml_1 caml_h01 true;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false caml_1 caml_h1 is true"
      ~ignore_health:false caml_1 caml_h01 false;
    (*Compared same caml of diff health*)
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true cactus_0 cactus_1 is \
       false"
      ~ignore_health:true cactus_0 cactus_1 false;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false cactus_0 cactus_1 is \
       false"
      ~ignore_health:false cactus_0 cactus_1 false;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false cactus_0 cactus_0 is \
       false"
      ~ignore_health:false cactus_0 cactus_0 true;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true cactus_0 cactus_0 is \
       false"
      ~ignore_health:true cactus_0 cactus_0 true;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true cactus_0 cactus_00 is \
       true"
      ~ignore_health:true cactus_0 cactus_00 true;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false cactus_0 cactus_00 is \
       false"
      ~ignore_health:false cactus_0 cactus_00 false
    (* Comparing different objects of same type Cactus*);
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false spit_0 spit_0 is true"
      ~ignore_health:false spit_0 spit_0 true;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true spit_0 spit_0 is true"
      ~ignore_health:true spit_0 spit_0 true;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:false spit_0 spit_1 is false"
      ~ignore_health:false spit_0 spit_1 false;
    compare_game_objects_test1
      "compare_game_objects ~ignore_health:true spit_0 spit_1 is false"
      ~ignore_health:true spit_0 spit_1 false;
    (*Comparing Spits against Spits*)
    compare_game_objects_test2
      "compare_game_objects cactus_0 caml_e0 is false" cactus_0 caml_e0
      false;
    compare_game_objects_test2
      "compare_game_objects cactus_0 spit_0 is false" cactus_0 spit_0
      false;
    compare_game_objects_test2
      "compare_game_objects cactus_0 python_0 is false" cactus_0
      python_0 false;
    compare_game_objects_test2
      "compare_game_objects camel_e0 spit_0 is false" caml_e0 spit_0
      false;
    compare_game_objects_test2
      "compare_game_objects camel_e0 python_0 is false" caml_e0 python_0
      false;
    compare_game_objects_test2
      "compare_game_objects python_0 spit_0 is false" python_0 spit_0
      false
    (*Comparing different game objects against other game objects*)
    (*End of compare_game_object, missing incr_spit_timer, update_pos*);
    can_spit_test "can_spit caml_e0 is true" caml_e0 true;
    can_spit_test "can_spit cactus_0 is true" cactus_0 false;
    can_spit_test "can_spit python_0 is true" python_0 false;
    can_spit_test "can_spit spit_0 is true" spit_0 false;
    can_spit_test "can_spit caml_h01 is true" caml_h01 true;
    can_spit_test "can_spit cactus_00 is true" cactus_00 false;
    can_spit_test "can_spit python_00 is true" python_00 false
    (*can_spit - missing incr_spit_timer, update_pos*);
    get_rain_rate_test "get_rain_rate caml_e0 is 0" caml_e0 0;
    get_rain_rate_test "get_rain_rate cactus_0 is 10" cactus_0 10;
    get_rain_rate_test "get_rain_rate python_0 is 0" python_0 0;
    get_rain_rate_test "get_rain_rate spit_0 is 0" spit_0 0;
    get_rain_rate_test "get_rain_rate caml_h01 is 0" caml_h01 0;
    get_rain_rate_test "get_rain_rate cactus_00 is 10" cactus_00 10;
    get_rain_rate_test "get_rain_rate python_00 is 0" python_00 0
    (*get_rain_rate - missing incr_spit_timer, update_pos*);
    get_object_points_test "get_object_points caml_e0 is 0" caml_e0 0;
    get_object_points_test "get_object_points cactus_0 is 0" cactus_0 0;
    get_object_points_test "get_object_points spit_0 is 0" spit_0 0;
    get_object_points_test "get_object_points python_0 is 100" python_0
      100;
    get_object_points_test "get_object_points caml_h01 is 0" caml_h01 0;
    get_object_points_test "get_object_points cactus_00 is 0" cactus_00
      0;
    get_object_points_test "get_object_points python_00 is 100"
      python_00 100
    (*get_object_points - missing incr_spit_timer, update_pos *);
    get_object_damage_test "get_object_damage caml_e0 is 0" caml_e0 0;
    get_object_damage_test "get_object_damage cactus_0 is 0" cactus_0 0;
    get_object_damage_test "get_object_damage python_0 is 2" python_0 2;
    get_object_damage_test "get_object_damage spit_0 is 1" spit_0 1;
    get_object_damage_test "get_object_damage caml_h01 is 0" caml_h01 0;
    get_object_damage_test "get_object_damage cactus_00 is 0" cactus_00
      0;
    get_object_damage_test "get_object_damage python_00 is 2" python_00
      2
    (*get_object_damage - missing incr_spit_timer, update_pos *);
    get_cost_test "get_cost caml_e0 is 100" caml_e0 100;
    get_cost_test "get_cost python_0 is 0" python_0 0;
    get_cost_test "get_cost spit_0 is 0" spit_0 0;
    get_cost_test "get_cost cactus_0 is 50" cactus_0 50;
    get_cost_test "get_cost caml_h01 is 100" caml_h01 100;
    get_cost_test "get_cost python_00 is 0" python_00 0;
    get_cost_test "get_cost cactus_00 is 50" cactus_00 50
    (*get_cost - missing incr_spit_timer, update_pos *);
    get_obj_position_test "get_object_position caml_e0 is (0,0)" caml_e0
      (0, 0);
    get_obj_position_test "get_object_position caml_e1 is (-2,-3)"
      caml_e1 (-2, -3);
    get_obj_position_test "get_object_position caml_1 is (1,1)" caml_1
      (1, 1);
    get_obj_position_test "get_object_position caml_10_6 is (10,6)"
      caml_10_6 (10, 6);
    get_obj_position_test "get_object_position caml_11_5 is (11,5)"
      caml_11_5 (11, 5);
    get_obj_position_test
      "get_object_position caml_max is (max_int, max_int)" caml_max
      (max_int, max_int);
    get_obj_position_test "get_object_position cactus_0 is (0,0)"
      cactus_0 (0, 0);
    get_obj_position_test "get_object_position cactus_1 is (1,1)"
      cactus_1 (1, 1);
    get_obj_position_test "get_object_position python_0 is (0,0)"
      python_0 (0, 0);
    get_obj_position_test "get_object_position python_1 is (1,1)"
      python_1 (1, 1);
    get_obj_position_test "get_object_position spit_0 is (0,0)" spit_0
      (0, 0);
    get_obj_position_test "get_object_position spit_1 is (1,1)" spit_1
      (1, 1)
    (*get_obj_position_test - notes: cannot test damage_character; not
      tested update_pos, incr_spit_timer*);
    get_obj_position_test "get_object_position pos_caml is (1,1)"
      pos_caml (1, 1);
    get_obj_position_test "get_object_position pos_cactus is (3,3)"
      pos_cactus (3, 3);
    get_obj_position_test
      "get_object_position pos_python is (max_int, max_int)" pos_python
      (max_int, max_int);
    get_obj_position_test "get_object_position pos_spit is (0,0)"
      pos_spit (0, 0);
    get_obj_position_test "get_object_position pos1_spit is (3, 3)"
      pos1_spit (3, 3);
    get_obj_position_test "get_object_position pos2_spit is (-10, -10)"
      pos2_spit (-10, -10);
    get_timer_test "get_timer of caml_1 is 0" caml_1 0;
    get_timer_test "get_timer of caml_incr is 2" caml_incr 2;
    get_timer_test "get_timer of caml_incr2 is 1" caml_incr2 1;
    get_timer_test "get_timer of caml_incr3 is 0" caml_incr3 0;
    get_timer_test "get_timer of python_0 is 0" python_0 0;
    get_timer_test "get_timer of spit_0 is 0" spit_0 0;
    get_health_test "get_health of cactus_0 is 10" cactus_0 10;
    get_health_test "get_health of cactus_helth is 5" cactus_helth 5;
    get_health_test "get_health of caml_e0 is 1" caml_e0 1;
    get_health_test "get_health of caml_helth is 1" caml_helth 1;
    get_health_test "get_health of python_0 is 2" python_0 2;
    get_health_test "get_health of python_helth is 1" python_helth 1;
  ]

let suite =
  "test suite for FINAL"
  >::: List.flatten [ state_tests; characters_test ]

let _ = run_test_tt_main suite
