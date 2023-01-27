(* gui.ml *)
open Src
open Graphics
open Png
open Characters
open Tile
open State

type int = color

type pic = {
  image : image;
  size : int * int;
}

let rel_x x =
  let x = float_of_int x in
  let sizeX = float_of_int (size_x ()) in
  int_of_float (x *. sizeX /. 500.)

let rel_y y =
  let y = float_of_int y in
  let sizeY = float_of_int (size_y ()) in
  int_of_float (y *. sizeY /. 500.)

let img_size img =
  let dump = dump_image img in
  Array.(length dump.(0), length dump)

let resize_img ?(use_avg = false) sizeX sizeY img =
  let iof = int_of_float in
  let foi = float_of_int in
  let ios x = int_of_string ("0x" ^ x) in
  let img2D = dump_image img in
  let imgX, imgY = Array.(length img2D.(0), length img2D) in
  let sizeX, sizeY = (rel_x sizeX, rel_y sizeY) in

  let avg_color (posi, posj) xfact yfact =
    let total = ref (0, 0, 0) in
    let count = ref 0 in

    for j = posj - yfact to posj + yfact do
      for i = posi - xfact to posi + xfact do
        if i > -1 && j > -1 && i < imgX && j < imgY then (
          (*Printf.sprintf "%X" 252;; Printf.sprintf "U+%04x" 123*)
          let color = Printf.sprintf "%06x" img2D.(j).(i) in

          (total :=
             match !total with
             | r, g, b ->
                 String.
                   ( r + ios (sub color 0 2),
                     g + ios (sub color 2 2),
                     b + ios (sub color 4 2) ));

          count := !count + 1)
      done
    done;

    if !count = 0 then -1
    else
      match !total with
      | r, g, b ->
          let to_str = Printf.sprintf "%02x" in
          ios
            (to_str (r / !count)
            ^ to_str (g / !count)
            ^ to_str (b / !count))
  in

  (* new size -> old size (inverted scale)*)
  let xScale, yScale = (foi imgX /. foi sizeX, foi imgY /. foi sizeY) in
  let xfactor, yfactor = (iof xScale, iof yScale) in

  let new2D = Array.make_matrix sizeY sizeX ~-1 in

  for j = 0 to sizeY - 1 do
    for i = 0 to sizeX - 1 do
      let newI, newJ = (iof (foi i *. xScale), iof (foi j *. yScale)) in
      new2D.(j).(i) <-
        (if not use_avg then img2D.(newJ).(newI)
        else avg_color (newI, newJ) xfactor yfactor)
    done
  done;

  make_image new2D

let draw_pic img x y = draw_image img (rel_x x) (rel_y y)

let draw_tile
    (upper_left : int * int)
    (upper_right : int * int)
    (lower_right : int * int)
    (lower_left : int * int)
    (color : color) =
  set_color color;
  let rect =
    [ upper_left; upper_right; lower_right; lower_left ]
    |> Array.of_list
  in
  fill_poly rect

let rec draw_row (lst : color list) (init_x : int) (init_y : int) =
  match lst with
  | [] -> ()
  | h :: t ->
      draw_tile
        (init_x |> rel_x, init_y + 100 |> rel_y)
        (init_x + 100 |> rel_x, init_y + 100 |> rel_y)
        (init_x + 100 |> rel_x, init_y |> rel_y)
        (init_x |> rel_x, init_y |> rel_y)
        h;
      draw_row t (init_x + 100) init_y

let draw_text (color : color) (s : string) (x : int) (y : int) =
  let oldX, oldY = current_point () in
  set_color color;
  moveto (rel_x x) (rel_y y);
  draw_string s;
  moveto oldX oldY

let start_text = "Press p to play or q to quit!"

(*~~~~~~~~~~~~~~~~~IMAGE LOADING~~~~~~~~~~~~~~~~~~*)
let _ = open_graph " 500x500"
let board_pos = ref (0, 0)
let tile_size = ref (0, 0)
let tile_num = ref (0, 0)
let old_x = ref (size_x ())
let old_y = ref (size_y ())
let game_timer = ref 0
let title1 = ref { image = create_image 1 1; size = (0, 0) }
let play1 = ref { image = create_image 1 1; size = (0, 0) }
let play2 = ref { image = create_image 1 1; size = (0, 0) }
let snake1 = ref { image = create_image 1 1; size = (0, 0) }
let heart1 = ref { image = create_image 1 1; size = (0, 0) }
let exit1 = ref { image = create_image 1 1; size = (0, 0) }
let backdrop = ref { image = create_image 1 1; size = (0, 0) }
let score_label = ref { image = create_image 1 1; size = (0, 0) }
let rain_label = ref { image = create_image 1 1; size = (0, 0) }
let lose_screen = ref { image = create_image 1 1; size = (0, 0) }
let win_screen = ref { image = create_image 1 1; size = (0, 0) }
let backdrop2 = ref { image = create_image 1 1; size = (0, 0) }
let camel_label1 = ref { image = create_image 1 1; size = (0, 0) }
let cactus_label1 = ref { image = create_image 1 1; size = (0, 0) }
let camel1 = ref { image = create_image 1 1; size = (0, 0) }
let spit1 = ref { image = create_image 1 1; size = (0, 0) }
let cactus1 = ref { image = create_image 1 1; size = (0, 0) }

let init_pic source sizeX sizeY =
  {
    image =
      Png.load_as_rgb24 source []
      |> Graphic_image.of_image |> resize_img sizeX sizeY;
    size = (rel_x sizeX, rel_y sizeY);
  }

let refresh_imgs () =
  title1 := init_pic "gui/pics/title1.png" 450 450;

  play1 := init_pic "gui/pics/play_button1.png" 100 100;

  play2 := init_pic "gui/pics/play_button2_new.png" 100 100;

  exit1 := init_pic "gui/pics/exit1.png" 25 40;

  snake1 := init_pic "gui/pics/snake1.png" 25 30;

  heart1 := init_pic "gui/pics/heart1.png" 40 40;

  backdrop := init_pic "gui/pics/backdrop1.png" 300 200;

  rain_label := init_pic "gui/pics/rain_label1.png" 20 30;

  score_label := init_pic "gui/pics/score_label1.png" 20 30;

  lose_screen := init_pic "gui/pics/lose1.png" 500 500;

  win_screen := init_pic "gui/pics/win1.png" 500 500;

  backdrop2 := init_pic "gui/pics/backdrop2.png" 100 250;

  camel_label1 := init_pic "gui/pics/camel_button1.png" 20 30;

  cactus_label1 := init_pic "gui/pics/cactus_button1.png" 18 35;

  spit1 := init_pic "gui/pics/spit1.png" 20 20;

  camel1 := init_pic "gui/pics/camel1.png" 20 30;

  cactus1 := init_pic "gui/pics/cactus1.png" 18 35

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*~~~~~~~~~~~~~~~~~~~BUTTONS~~~~~~~~~~~~~~~~~~~~~~*)

type button = {
  pic : pic ref;
  pos : int * int;
}

let play_button = ref { pic = play1; pos = (0, 0) }
let play_again_button = ref { pic = play2; pos = (0, 0) }
let exit_button = ref { pic = exit1; pos = (0, 0) }
let camel_button = ref { pic = camel_label1; pos = (0, 0) }
let cactus_button = ref { pic = cactus_label1; pos = (0, 0) }

let button_ref_list =
  [
    play_button;
    exit_button;
    camel_button;
    cactus_button;
    play_again_button;
  ]

let draw_button button x y =
  button := { !button with pos = (rel_x x, rel_y y) };
  draw_pic !(!button.pic).image x y

let button_from_pos (mouseX, mouseY) =
  if
    mouseX < 0 || mouseY < 0
    || mouseX >= size_x ()
    || mouseY >= size_y ()
  then None
  else
    try
      Some
        (List.find
           (fun b ->
             let x, y = !b.pos in
             let sizeY, sizeX = !(!b.pic).size in
             mouseX > x
             && mouseX < x + sizeX
             && mouseY > y
             && mouseY < y + sizeY)
           button_ref_list)
    with
    | _ -> None

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let start_game = ref (fun () -> ())

let draw_board
    ?(mouse_hover = true)
    board
    (posX, posY)
    boardsizeX
    boardsizeY =
  let boardsizeX, boardsizeY = (rel_x boardsizeX, rel_y boardsizeY) in
  let tileX, tileY = List.(board |> hd |> length, length board) in
  let posX, posY = (rel_x posX, rel_y posY) in

  let tilesizeX, tilesizeY = (boardsizeX / tileX, boardsizeY / tileY) in

  board_pos := (posX, posY);
  tile_size := (tilesizeX, tilesizeY);
  tile_num := (tileX, tileY);

  let color_hover =
    let mouse_x, mouse_y =
      match mouse_pos () with
      | x, y -> (x - posX, y - posY)
    in
    let foi = float_of_int in
    let posX, posY =
      (foi mouse_x /. foi boardsizeX, foi mouse_y /. foi boardsizeY)
    in

    let tile_x, tile_y =
      ( int_of_float (posX *. foi tileX),
        int_of_float (posY *. foi tileY) )
    in

    (*should range 0-4*)
    if mouse_x < 0 || mouse_y < 0 || posX >= 1. || posY >= 1. then
      (-1, -1)
    else (tile_x, tile_y)
  in

  let yellow = 0xFFC95E in
  let orange = 0xffae00 in
  let c = ref yellow in

  for j = 0 to tileY - 1 do
    if tileY mod 2 = 1 then
      if !c = yellow then c := orange else c := yellow;
    for i = 0 to tileX - 1 do
      if !c = yellow then c := orange else c := yellow;
      let color =
        match color_hover with
        | h_x, h_y -> if h_x = i && h_y = j then blue else !c
      in
      draw_tile
        (posX + (i * tilesizeX), posY + ((j + 1) * tilesizeY))
        (posX + ((i + 1) * tilesizeX), posY + ((j + 1) * tilesizeY))
        (posX + ((i + 1) * tilesizeX), posY + (j * tilesizeY))
        (posX + (i * tilesizeX), posY + (j * tilesizeY))
        color
    done
  done

let draw_objects state =
  let objs = get_objects state in

  List.iter
    (fun o ->
      let tileY, tileX = get_obj_position o in
      let (bx, by), (tx, ty) = (!board_pos, !tile_size) in

      let posX, posY =
        (bx + (tx * (tileX - 1)), by + (ty * (tileY - 1)))
      in

      if object_string o = "Py" then
        let imgX, imgY = !snake1.size in
        let offsetX, offsetY = ((tx - imgX) / 2, (ty - imgY) / 2) in
        draw_image !snake1.image (posX + offsetX) (posY + offsetY)
      else if object_string o = "Caml" then
        let imgX, imgY = !camel1.size in
        let offsetX, offsetY = ((tx - imgX) / 2, (ty - imgY) / 2) in
        draw_image !camel1.image (posX + offsetX) (posY + offsetY)
      else if object_string o = "*" then
        let imgX, imgY = !spit1.size in
        let offsetX, offsetY = ((tx - imgX) / 2, (ty - imgY) / 2) in
        draw_image !spit1.image (posX + offsetX) (posY + offsetY)
      else if object_string o = "Cact" then
        let imgX, imgY = !cactus1.size in
        let offsetX, offsetY = ((tx - imgX) / 2, (ty - imgY) / 2) in
        draw_image !cactus1.image (posX + offsetX) (posY + offsetY))
    objs

let draw_health state =
  let imgX, _ =
    match !heart1.size with
    | x, y -> (x + rel_x 10, y)
  in
  for i = 0 to state_life state - 1 do
    draw_image !heart1.image (rel_x 175 + (imgX * i)) 3
  done

let draw_state state =
  draw_board (get_tiles state) (110, 50) 300 250;
  draw_objects state;
  draw_health state;
  draw_text cyan (state |> get_raindrops |> string_of_int) 190 377;
  draw_text green (state |> get_score |> string_of_int) 190 337;
  draw_text blue
    (state |> get_state_level |> string_of_int |> ( ^ ) "Level: ")
    230 415

let draw_UI () =
  draw_button exit_button 10 450;
  draw_pic !backdrop.image 100 300;
  draw_pic !backdrop2.image 5 50;
  draw_pic !rain_label.image 160 370;
  draw_pic !score_label.image 160 330;
  let _ =
    draw_button camel_button 35 250;
    draw_text red (init_caml 1 0 0 |> get_cost |> string_of_int) 65 255;
    draw_button cactus_button 35 210;
    draw_text red (init_cactus 0 0 |> get_cost |> string_of_int) 65 215
  in
  ()

let clear_screen () =
  clear_graph ();
  set_color 0x774620;
  fill_rect 0 0 (size_x ()) (size_y ())

let play_again () =
  clear_screen ();
  let redo_ui () =
    draw_button play_again_button 200 200;
    draw_button exit_button 10 450
  in

  redo_ui ();

  let rec wait_loop () =
    if !old_x <> size_x () || !old_y <> size_y () then (
      old_x := size_x ();
      old_y := size_y ();
      refresh_imgs ();
      clear_screen ();
      redo_ui ());

    let redo () =
      Unix.sleepf 0.05;
      wait_loop ()
    in

    if button_down () then
      match button_from_pos (mouse_pos ()) with
      | Some b ->
          if b == play_again_button then (
            !start_game ();
            exit 0)
          else if b == exit_button then exit 0
          else redo ()
      | None -> redo ()
    else redo ()
  in

  wait_loop ()

let next_state state =
  try
    let rand = Random.int 2 in
    state |> check_state_win |> handle_collisions [] |> move_spit_state
    |> handle_collisions [] ~spit_only:true
    |> move_python_state |> camel_spit_state |> spawn_python_state rand
    |> add_raindrops 5 |> cactus_rain_state
  with
  | Win st ->
      if get_state_level st >= 3 then (
        clear_screen ();
        draw_UI ();
        draw_state st;
        Unix.sleep 2;
        clear_screen ();
        draw_pic !win_screen.image 0 0;
        Unix.sleep 3;
        play_again ())
      else next_state_level st
  | Lose st ->
      clear_screen ();
      draw_UI ();
      draw_state st;
      Unix.sleep 2;
      clear_screen ();
      draw_pic !lose_screen.image 0 0;
      Unix.sleep 3;
      play_again ()

let is_dragging = ref false
let drag_pic = ref { image = create_image 1 1; size = (0, 0) }
let select_char = ref ""

let get_tile_from_pos (posX, posY) =
  let (bx, by), (tx, ty), (nx, ny) =
    (!board_pos, !tile_size, !tile_num)
  in
  if
    posX <= bx || posY <= by
    || posX >= bx + (tx * nx)
    || posY >= by + (ty * ny)
  then None
  else Some (((posX - bx) / tx) + 1, ((posY - by) / ty) + 1)

let make_character label pos =
  match get_tile_from_pos pos with
  | Some (x, y) ->
      if label == "Caml" then Some (init_caml 1 y x)
      else if label == "Cact" then Some (init_cactus y x)
      else None
  | None -> None

let add_character state label pos =
  match make_character label pos with
  | None -> state
  | Some ch -> (
      try buy_object ch state with
      | _ -> state)

let rec play_loop state =
  if !old_x <> size_x () || !old_y <> size_y () then (
    old_x := size_x ();
    old_y := size_y ();
    refresh_imgs ());

  game_timer := !game_timer + 1;
  clear_screen ();
  draw_UI ();
  draw_state state;

  let state =
    if !is_dragging then (
      let sizeX, sizeY = !drag_pic.size in
      let mouseX, mouseY = mouse_pos () in
      draw_image !drag_pic.image
        (mouseX - (sizeX / 2))
        (mouseY - (sizeY / 2));

      if not (button_down ()) then (
        let nstate =
          add_character state !select_char (mouseX, mouseY)
        in
        is_dragging := false;
        select_char := "";
        drag_pic := { image = create_image 1 1; size = (0, 0) };

        nstate)
      else state)
    else if button_down () then
      let mp = mouse_pos () in
      match button_from_pos mp with
      | Some b ->
          if b == exit_button then exit 0
          else if b == camel_button then (
            is_dragging := true;
            drag_pic := !camel_label1;
            select_char := "Caml";
            state)
          else if b == cactus_button then (
            is_dragging := true;
            drag_pic := !cactus_label1;
            select_char := "Cact";
            state)
          else state
      | None -> state
    else state
  in

  Unix.sleepf 0.05;

  if !game_timer mod 20 = 0 then state |> next_state |> play_loop
  else play_loop state

let _ =
  (start_game :=
     fun _ ->
       draw_text 0 "Click to begin!" 200 200;

       let _ = wait_next_event [ Button_down ] in

       clear_screen ();

       refresh_imgs ();

       let intro_screen () =
         draw_pic !title1.image 20 30;
         draw_button play_button 200 100;
         draw_button exit_button 10 450
       in

       intro_screen ();

       let rec waitLoop () =
         if !old_x <> size_x () || !old_y <> size_y () then (
           old_x := size_x ();
           old_y := size_y ();
           refresh_imgs ();

           clear_screen ();
           intro_screen ());

         let redo () =
           Unix.sleepf 0.05;
           waitLoop ()
         in

         if button_down () then
           match button_from_pos (mouse_pos ()) with
           | Some b ->
               if b == play_button then clear_screen ()
               else if b == exit_button then exit 0
               else redo ()
           | None -> redo ()
         else redo ()
       in

       old_x := size_x ();
       old_y := size_y ();
       waitLoop ();

       init_state 5 10 ~raindrops:250 ~score:490
       |> next_state_level |> play_loop);

  !start_game ()
