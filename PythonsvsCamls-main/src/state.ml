open Characters
open Tile

type objects_state = {
  tiles : tile list list;
  lives : int;
  remaining_py : int;
  score : int;
  raindrops : int;
  level : int;
}

let print_red = ANSITerminal.print_string [ ANSITerminal.red ]
let print_green = ANSITerminal.print_string [ ANSITerminal.green ]
let print_yellow = ANSITerminal.print_string [ ANSITerminal.yellow ]
let print_magenta = ANSITerminal.print_string [ ANSITerminal.magenta ]
let print_cyan = ANSITerminal.print_string [ ANSITerminal.cyan ]

exception OccupiedTile of string
exception UnoccupiedTile of string
exception Lose of objects_state
exception Win of objects_state
exception Price

type result =
  | Legal of objects_state
  | Illegal

let init_state
    ?(score = 0)
    ?(num_py = 0)
    ?(level = 0)
    ?(raindrops = 0)
    size_x
    size_y =
  {
    tiles = make_board (0, 0) size_x size_y;
    lives = 3;
    score;
    remaining_py = num_py;
    raindrops;
    level;
  }

let next_state_level state =
  let num_py_of_lvl lvl = (3 * lvl) + 2 in

  let raindrops_of_lvl lvl = (50 * lvl) + 200 in

  init_state
    (List.length state.tiles)
    List.(state.tiles |> hd |> length)
    ~score:state.score ~level:(state.level + 1)
    ~num_py:(num_py_of_lvl (state.level + 1) + state.remaining_py)
    ~raindrops:(raindrops_of_lvl (state.level + 1) + state.raindrops)

let add_raindrops num state =
  { state with raindrops = state.raindrops + num }

let get_raindrops st = st.raindrops
let get_state_level state = state.level

let spend_raindrops num state =
  if num > state.raindrops then raise Price;
  { state with raindrops = state.raindrops - num }

let print_score state =
  print_string "\nTotal Score: ";
  print_green (string_of_int state.score ^ "\n")

let get_score state = state.score

let print_level state =
  print_string "Ready? Starting level ";
  print_green (string_of_int state.level ^ "...\n")

let print_state state =
  print_endline "";
  print_board state.tiles;
  print_string "Lives: ";
  let _ = List.init state.lives (fun _ -> print_red "<3 ") in
  (*❤️*)
  print_string "\nRaindrops: ";
  print_cyan (string_of_int state.raindrops ^ "\n")

let state_life st = st.lives

let lose_life state num =
  if state.lives - num < 1 then raise (Lose { state with lives = 0 })
  else { state with lives = state.lives - num }

let get_tiles state = state.tiles
let move_spit_state state = { state with tiles = move_spit state.tiles }

let move_python_state state =
  let new_tiles, lives = move_py state.tiles in
  lose_life { state with tiles = new_tiles } lives

let camel_spit_state state =
  { state with tiles = caml_spit state.tiles }

let check_state_win state =
  if
    state.remaining_py < 1
    && not
         List.(
           exists
             (fun row -> exists (check_if_on_tile "Py") row)
             state.tiles)
  then raise (Win state)
  else state

let spawn_python_state num state =
  let new_num = if state.remaining_py - num < 0 then 0 else num in

  {
    state with
    tiles = spawn_python new_num state.tiles;
    remaining_py = state.remaining_py - new_num;
  }

let get_objects state =
  List.fold_left
    (fun lst row ->
      lst @ List.fold_left (fun lst' t -> lst' @ chars_on_tile t) [] row)
    [] state.tiles

let get_camls state =
  List.filter (fun x -> object_string x = "Caml") (get_objects state)

let get_pythons state =
  List.filter (fun x -> object_string x = "Py") (get_objects state)

(*Gets all cacti from state*)
let get_cacti state =
  List.filter (fun x -> object_string x = "Cact") (get_objects state)

let get_spits state =
  List.filter (fun x -> object_string x = "*") (get_objects state)

let cactus_rain_state state =
  let total_rain =
    state |> get_cacti
    |> List.fold_left (fun tot c -> tot + get_rain_rate c) 0
  in
  add_raindrops total_rain state

(** [] raises TileConflict if invalid placement*)
let add_object (o : game_objects) (state : objects_state) =
  { state with tiles = add_object_to_board o state.tiles }

let buy_object o state =
  let new_state = state |> spend_raindrops (get_cost o) in
  add_object o new_state

let remove_object (o : game_objects) (state : objects_state) =
  { state with tiles = remove_object_from_board o state.tiles }

(**THROWS EXCEPTION Not_Found*)
let get_two_collisions ?(spit_only = false) tile exclude_lst =
  let py =
    List.find (fun x -> object_string x = "Py") (chars_on_tile tile)
  in
  let c =
    List.find
      (fun x ->
        let s = object_string x in

        if spit_only then s = "*"
        else s = "*" || s = "Cact" || s = "Caml")
      (chars_on_tile tile)
  in

  if
    List.(
      exists
        (fun o ->
          compare_game_objects o py ~ignore_health:true
          && compare_game_objects o c ~ignore_health:true)
        exclude_lst)
  then failwith ":P";
  (py, c)

let state_add_points num state =
  { state with score = state.score + num }

let rec handle_collisions
    ?(spit_only = false)
    (dealt_damage : game_objects list)
    (o : objects_state) : objects_state =
  let rec multiple_obj ob =
    match ob.tiles with
    | [] -> []
    | r :: t2 -> begin
        match r with
        | [] -> multiple_obj { o with tiles = t2 }
        | x :: t -> (
            match get_two_collisions x dealt_damage ~spit_only with
            | p, c -> (p, c) :: multiple_obj { o with tiles = t :: t2 }
            | exception _ -> multiple_obj { o with tiles = t :: t2 })
      end
  in

  let collision_lst = multiple_obj o in

  if
    List.length collision_lst = 0
    || List.(
         exists
           (fun (p, c) ->
             object_string c <> "*"
             && exists
                  (fun obj ->
                    compare_game_objects obj p ~ignore_health:true)
                  dealt_damage)
           collision_lst)
  then o
  else
    let new_state =
      List.fold_left
        (fun s (p, c) ->
          let new_p = damage_character p (get_object_damage c) in

          let new_c =
            if
              List.exists
                (fun o -> compare_game_objects o p ~ignore_health:true)
                dealt_damage
              && object_string c <> "*"
            then Some c
            else damage_character c (get_object_damage p)
          in

          let state = s |> remove_object p |> remove_object c in

          match (new_p, new_c) with
          | None, None ->
              state |> state_add_points (get_object_points p)
          | Some p, None -> state |> add_object p
          | None, Some c ->
              state |> add_object c
              |> state_add_points (get_object_points p)
          | Some p, Some c -> state |> add_object p |> add_object c)
        o collision_lst
    in

    handle_collisions
      (List.fold_left
         (fun lst (x1, x2) -> x1 :: x2 :: lst)
         dealt_damage collision_lst)
      new_state
