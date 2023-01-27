(* tile.mli *)
(** Representation of the tiles which make up a game board *)

exception TileConflict of string
(** Raised when user tries to place an object on a tile which is already
    occupied or is out of bounds of the game board. *)

type tile
(** [tile] represents a single square in a grid, which will be used to
    make up the board*)

val get_position : tile -> int * int
(**[get_position t] returns the position of tile t*)

val is_occupied : tile -> bool
(**[is_occupied t] returns whether t is occupied*)

val chars_on_tile : tile -> Characters.game_objects list
(** [chars_on_tile t] will return a list of all the characters currently
    exist on a tile. For example, an unoccupied tile will return [], a
    tile with just a cactus would return a list with a single Cactus
    game_object in it. *)

val compare_position : int * int -> int * int -> bool
(** [compare_position (x1, x2) (y1, y2)] will compare two given
    coordinates and return true if they are the same, and false
    otherwise. For example, compare_position (1,1) (1,1) returns true,
    however compare_position (3,1), (2,2) returns false. Requires: x1,
    x2, y1, y2 are all within the bounds of the current board. *)

val make_board : int * int -> int -> int -> tile list list
(** [make_board pos x y] returns a 2D list of tiles of dimensions x by y
    centered about position pos*)

val make_tile :
  int * int -> bool -> Characters.game_objects list -> tile
(** [make_tile pos occ objs] creates a new tile with center_point pos,
    occupacy occ, and objects obj*)

val print_board : tile list list -> unit
(** [print_board board] prints a board, marking places where there is an
    occupied character*)

val add_object_to_board :
  Characters.game_objects -> tile list list -> tile list list
(** [add_object_to_board obj board] returns a board with [obj] placed at
    the position described by the obj record. *)

val remove_object_from_board :
  Characters.game_objects -> tile list list -> tile list list
(** [remove_object_to_board obj board] returns a board with [obj]
    removed from the position described by the obj record. *)

val move_spit : tile list list -> tile list list
(** [move_spit b] moves every single spit object on board [b]
    horizontally to the right by one tile after one state change. *)

val move_py : tile list list -> tile list list * int
(** [move_py b] moves every single python object on board [b]
    horizontally to the right by one tile after one state change. This
    is in the form of a tuple: (tile 2D list, number of finished
    pythons) *)

val check_if_on_tile : string -> tile -> bool
(**[check_if_on_tile t expected] is whether tile t contains a
   game_object with name type expected*)

val caml_spit : tile list list -> tile list list
(** [caml_spit b] is the 2D tile list of the board [b] after all ready
    camels spit. All camel spit timers are updated accordingly.*)

val spawn_python : int -> tile list list -> tile list list
(** [spawn_python i b] is the board [b] after [i] pythons have been
    randomly spawned on the rightmost side of [b]*)
