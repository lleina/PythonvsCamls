open Src
open Characters
open Tile
open State

let print_red = ANSITerminal.print_string [ ANSITerminal.red ]
let print_green = ANSITerminal.print_string [ ANSITerminal.green ]
let print_yellow = ANSITerminal.print_string [ ANSITerminal.yellow ]
let print_magenta = ANSITerminal.print_string [ ANSITerminal.magenta ]
let print_cyan = ANSITerminal.print_string [ ANSITerminal.cyan ]

let logo_string =
  "\n\n\
  \ ▄▄·  ▄▄▄· • ▌ ▄ ·. ▄▄▌  .▄▄ ·      ▌ ▐·.▄▄ ·      ▄▄▄· ▄· ▄▌▄▄▄▄▄ \
   ▄ .▄       ▐ ▄ .▄▄ ·     \n\
   ▐█ ▌▪▐█ ▀█ ·██ ▐███▪██•  ▐█ ▀.     ▪█·█▌▐█ ▀.     ▐█ ▄█▐█▪██▌•██  \
   ██▪▐█▪     •█▌▐█▐█ ▀.     \n\
   ██ ▄▄▄█▀▀█ ▐█ ▌▐▌▐█·██▪  ▄▀▀▀█▄    ▐█▐█•▄▀▀▀█▄     ██▀·▐█▌▐█▪ \
   ▐█.▪██▀▐█ ▄█▀▄ ▐█▐▐▌▄▀▀▀█▄    \n\
   ▐███▌▐█ ▪▐▌██ ██▌▐█▌▐█▌▐▌▐█▄▪▐█     ███ ▐█▄▪▐█    ▐█▪·• ▐█▀·. \
   ▐█▌·██▌▐▀▐█▌.▐▌██▐█▌▐█▄▪▐█    \n\
   ·▀▀▀  ▀  ▀ ▀▀  █▪▀▀▀.▀▀▀  ▀▀▀▀     . ▀   ▀▀▀▀     .▀     ▀ •  ▀▀▀ \
   ▀▀▀ · ▀█▄▀▪▀▀ █▪ ▀▀▀▀     \n\
  \ "

let clean str : string list =
  List.filter
    (fun x -> String.length x > 0)
    (String.split_on_char ' ' str)

exception InvalidInput of string

let parse str =
  match clean str with
  | [ "camel"; y; x ] -> init_caml 1 (int_of_string x) (int_of_string y)
  | [ "spit"; y; x ] -> init_spit (int_of_string x) (int_of_string y)
  | [ "cactus"; y; x ] ->
      init_cactus (int_of_string x) (int_of_string y)
  | [ "python"; y; x ] ->
      init_python (int_of_string x) (int_of_string y)
  | [ "score" ] -> raise (InvalidInput "score")
  | [] -> raise (InvalidInput "")
  | [ x ] -> raise (InvalidInput x)
  | _ -> raise (InvalidInput "Bad!")

let next_level state =
  let next_state = next_state_level state in
  print_level next_state;
  next_state

let next_state state =
  try
    let rand = Random.int 2 in
    State.(
      state |> check_state_win |> handle_collisions []
      |> move_spit_state
      |> handle_collisions [] ~spit_only:true
      |> move_python_state |> camel_spit_state
      |> spawn_python_state rand |> add_raindrops 5 |> cactus_rain_state)
  with
  | Lose st ->
      print_state st;
      print_score st;
      print_red "Aw, me-thinks you lost! A shame, innit!\n";
      exit 0
  | Win st ->
      print_state st;

      if get_state_level st = 3 then (
        print_score st;
        print_green "You won! Say it with me now: GEEE GEEEEE \n";
        exit 0)
      else next_level st
  | _ ->
      print_endline "uh oh!";
      state

let rec play state =
  state |> print_state;

  print_string "Enter command: ";
  print_yellow "[type] [position X] [positionY]\n";

  match parse (read_line ()) with
  | exception InvalidInput "quit" ->
      print_magenta "Quitting game. Bye Bye :(\n"
  | exception InvalidInput "score" ->
      print_score state;
      play state
  | exception InvalidInput "" -> state |> next_state |> play
  | exception InvalidInput _ ->
      print_red "Wong inpwut >:3\n";
      play state
  | x -> (
      try buy_object x state |> play with
      | TileConflict "Cact/Caml" ->
          print_red "Already a Cactus/Camel here!\n";
          play state
      | TileConflict "Bound" ->
          print_red "This is out of bounds!\n";
          play state
      | Price ->
          print_red "Insufficient Raindrops!\n";
          play state
      | _ ->
          print_endline "That wasn't supposed to happen!";
          play state)

let _ =
  print_endline
    "You may place a camel, cactus, python, or spit. Or, enter quit to \
     end the game";
  print_endline "X can range from 1 to 5, Y can range from 1 to 10";
  print_green (logo_string ^ "\n");
  next_level (init_state 5 10) |> play
