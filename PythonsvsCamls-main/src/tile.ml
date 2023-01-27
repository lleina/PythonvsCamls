(* tile.ml *)

let print_red = ANSITerminal.print_string [ ANSITerminal.red ]
let print_green = ANSITerminal.print_string [ ANSITerminal.green ]
let print_yellow = ANSITerminal.print_string [ ANSITerminal.yellow ]
let print_magenta = ANSITerminal.print_string [ ANSITerminal.magenta ]
let print_cyan = ANSITerminal.print_string [ ANSITerminal.cyan ]
let board_color = print_magenta

open Characters

exception TileConflict of string

type tile = {
  center_point : int * int;
  occupied : bool;
  objects : game_objects list;
}

let get_position t = t.center_point
let is_occupied t = t.occupied
let chars_on_tile t = t.objects

let compare_position (pos_x1, pos_y1) (pos_x2, pos_y2) =
  pos_x1 = pos_x2 && pos_y1 = pos_y2

let make_tile pos occ objs =
  { center_point = pos; occupied = occ; objects = objs }

let make_board (pos_x, pos_y) size_x size_y =
  let rec make_map map_x map_y map =
    let new_tile = make_tile (pos_x + map_x, pos_y + map_y) false [] in
    if map_x <= 0 || (map_x <= 1 && map_y <= 0) then map
    else if map_y <= 0 then make_map (map_x - 1) size_y ([] :: map)
    else
      let new_row = new_tile :: List.hd map in
      match map with
      | [] -> map
      | _ :: map_tail -> make_map map_x (map_y - 1) (new_row :: map_tail)
  in

  make_map size_x size_y [ [] ]

let print_board board =
  let max_x = List.length board in
  let max_y = List.length (List.hd board) in

  print_endline
    (List.fold_left ( ^ ) "  "
       (List.init max_y (fun i -> string_of_int (i + 1) ^ "  ")));

  let rec print_board_rec = function
    | [] -> print_endline ""
    | (til :: row) :: t ->
        if List.length row + 1 = max_y then
          print_int (max_x - List.length t);
        board_color "[";
        if List.length til.objects != 0 then
          List.iter
            (fun o ->
              print_object o;
              print_string ";")
            til.objects
        else print_string " ";
        board_color "]";
        print_board_rec (row :: t)
    | [] :: t ->
        print_endline "";
        print_board_rec t
  in

  print_board_rec board

(* helper function *)
let rec valid_tile_addition o obj_list : bool =
  let str_object_list = List.map (fun x -> object_string x) obj_list in
  not
    ((object_string o = "Cact" || object_string o = "Caml")
    && List.exists (fun s -> s = "Caml" || s = "Cact") str_object_list)
(* helper function *)

let add_object_to_board o board =
  let pos_x, pos_y = get_obj_position o in

  if
    pos_x <= 0 || pos_y <= 0
    || pos_x > List.length board
    || pos_y > List.(board |> hd |> length)
  then raise (TileConflict "Bound");

  let row = List.nth board (pos_x - 1) in
  let tile = List.nth row (pos_y - 1) in

  let newTile = { tile with objects = o :: tile.objects } in
  List.map
    (fun row ->
      List.map
        (fun t ->
          if compare_position (pos_x, pos_y) t.center_point then
            if valid_tile_addition o t.objects then newTile
            else raise (TileConflict "Cact/Caml")
          else t)
        row)
    board

let remove_object_from_board o board =
  let pos_x, pos_y = get_obj_position o in
  List.map
    (fun row ->
      List.map
        (fun t ->
          if compare_position (pos_x, pos_y) t.center_point then
            {
              t with
              objects =
                (let rec find = function
                   | [] -> []
                   | obj :: t ->
                       if compare_game_objects o obj then t
                       else obj :: find t
                 in

                 find t.objects);
            }
          else t)
        row)
    board

let move_spit board =
  List.map
    (fun row ->
      let is_spit x =
        match object_string x with
        | "*" -> true
        | _ -> false
      in

      let rec shift_spit new_spits = function
        | [] -> []
        | board_tile :: tail ->
            {
              board_tile with
              objects =
                (new_spits
                |> List.map (fun x ->
                       update_pos board_tile.center_point x))
                @ List.filter
                    (fun x -> not (is_spit x))
                    board_tile.objects;
            }
            :: shift_spit
                 List.(
                   board_tile.objects |> filter (fun x -> is_spit x))
                 tail
      in
      shift_spit [] row)
    board

let move_py board =
  let match_py x =
    match object_string x with
    | "Py" -> true
    | _ -> false
  in

  let is_plr_obj x =
    match object_string x with
    | "Caml"
    | "Cact" ->
        true
    | _ -> false
  in

  let num_finished_py =
    try
      List.(
        board |> map hd
        |> fold_left
             (fun row_count x ->
               (filter
                  (fun o ->
                    match_py o && not (exists is_plr_obj x.objects))
                  x.objects
               |> length)
               + row_count)
             0)
    with
    | _ -> 0
  in

  let new_board =
    List.map
      (fun row ->
        let rec shift_py new_py = function
          | [] -> []
          | board_tile :: tail ->
              {
                board_tile with
                objects =
                  (new_py
                  |> List.map (fun x ->
                         update_pos board_tile.center_point x))
                  @ List.filter
                      (fun x ->
                        (not (match_py x))
                        || List.exists is_plr_obj board_tile.objects)
                      board_tile.objects;
              }
              :: shift_py
                   List.(
                     board_tile.objects
                     |> filter (fun x ->
                            match_py x
                            && not
                                 (exists is_plr_obj board_tile.objects)))
                   tail
        in
        List.rev (shift_py [] (List.rev row)))
      board
  in

  (new_board, num_finished_py)

(** helper functions*)
let check_if_on_tile (expected : string) (t : tile) =
  List.exists (fun obj -> object_string obj = expected) t.objects
(**end helper functions*)

let register_damage board =
  List.(
    map
      (fun x ->
        map
          (fun t ->
            {
              t with
              objects =
                filter
                  (fun x ->
                    match object_string x with
                    | "Sp" -> not (check_if_on_tile "Py" t)
                    | "Py" -> not (check_if_on_tile "Sp" t)
                    | _ -> true)
                  t.objects;
            })
          x)
      board)

let caml_spit board =
  List.map
    (fun row ->
      List.map
        (fun til ->
          {
            til with
            objects =
              List.fold_left
                (fun lst o ->
                  let pos_x, pos_y = get_obj_position o in
                  if can_spit o then
                    init_spit pos_x pos_y :: incr_spit_timer o :: lst
                  else incr_spit_timer o :: lst)
                [] (chars_on_tile til);
          })
        row)
    board

let rec spawn_python i board =
  match i with
  | 0 -> board
  | n ->
      let row_len = List.length board in
      let col_len = List.(length (hd board)) in
      let rand = Random.int row_len + 1 in

      spawn_python (n - 1)
        (add_object_to_board (init_python rand col_len) board)
