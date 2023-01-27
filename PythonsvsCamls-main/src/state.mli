(* state.mli *)

(** Representation of dynamic character state. This module represents
    the state of a character as it is being played, including the
    characters's health, //**BLAH BLAH BLAH**// and functions that cause
    the state to change. *)

exception OccupiedTile of string
(** Raised when a character cannot be added to an occupied tile. *)

exception UnoccupiedTile of string
(** Raised when a character cannot be removed to an occupied tile. *)

type objects_state
(** The abstract type represents all the objects on the board during a
    single state, as well as a list of all the tiles.*)

exception Lose of objects_state
(** Raised when the player's lives are less than 1. *)

exception Win of objects_state
(** Raised when there are no more pythons to be spawned and the board is
    clear of pythons. *)

exception Price
(** Raised when the player attempts to buy an item with a cost higher
    than their current raindrops. *)

type result =
  | Legal of objects_state
  | Illegal
      (** The type representing whether or not an impending state is
          valid or not, *)

val init_state :
  ?score:int ->
  ?num_py:int ->
  ?level:int ->
  ?raindrops:int ->
  int ->
  int ->
  objects_state
(** [init_state ?sc ?n ?l ?r sizeX sizeY] is the intial state of the
    game. In this state, the score is set to [sc] and [n] is the total
    number of pythons that will spawn in the game. [r] is the number of
    raindrops at the start of the game. There are no objects on the
    board and no occupied tiles. [sizeX] is the number of horizontal
    grids and [sizeY] is the number of vertical grids in the intial
    board. Starts at level 0, otherwise at [l]. All optional inputs are
    defaulted to 0 when not entered. All states start out with 3 lives.
    Requires all inputs to be nonnegative. *)

val next_state_level : objects_state -> objects_state
(** [next_level state] is the initial state of the level after the level
    in state [state]. The level of this new state is one plus the level
    of [state]. The raindrop is calculated by adding 200 to 50 times the
    new level.*)

val add_raindrops : int -> objects_state -> objects_state
(** [add_raindrops num state] is the state after adding num raindrops to
    state. *)

val get_raindrops : objects_state -> int
(** [get_raindrops st] is the number of raindrops in objects_state [st]. *)

val get_state_level : objects_state -> int
(** [get_state_level st] is the current level of objects_state [st]. *)

val print_score : objects_state -> unit
(**[print_state st] prints the score of object_state state*)

val get_score : objects_state -> int
(** [get_score st] is the score current score in state [st]. *)

val print_level : objects_state -> unit
(**[print_level st] prints the current level of object_state state*)

val print_state : objects_state -> unit
(** [print_state st] prints objects_state [st] properties, including the
    board. *)

val state_life : objects_state -> int
(** [state_life st] is objects_state [st]'s current amount of lives
    left. *)

val lose_life : objects_state -> int -> objects_state
(** [lose_life state num] is the state with num less lives. Raises
    [Lose] exception when the new state has a life of 0 or less. *)

val get_tiles : objects_state -> Tile.tile list list
(** [get_tiles o] represents the game board.*)

val move_spit_state : objects_state -> objects_state
(** [move_spit_state st] is the state after all spits on the tiles move. *)

val move_python_state : objects_state -> objects_state
(** [move_python_state st] is the state after all pythons on the tiles
    move. *)

val camel_spit_state : objects_state -> objects_state
(** [move_spit_state st] is the state after camels spit. *)

val check_state_win : objects_state -> objects_state
(** [check_state_win state] raises Win if the win condition is met: no
    pythons to spawn and no pythons on the board.*)

val spawn_python_state : int -> objects_state -> objects_state
(** [spawn_python_state] is the state after pythons are randomly spawned
    on the board. *)

val get_objects : objects_state -> Characters.game_objects list
(** [get_objects state] is the list of every object in state*)

val get_camls : objects_state -> Characters.game_objects list
(** [get_camls state] is the list of all the camls inside of [state]. *)

val get_pythons : objects_state -> Characters.game_objects list
(** [get_pythons state] is the list of all the pythons inside of
    [state]. *)

val get_cacti : objects_state -> Characters.game_objects list
(** [get_cacti state] is the list of all the cacti inside of [state]. *)

val get_spits : objects_state -> Characters.game_objects list
(** [get_spits state] is the list of all the spits inside of [state]. *)

val cactus_rain_state : objects_state -> objects_state
(** [cactus_rain_state] is the state after all rain has been collected
    by the cacti*)

val add_object :
  Characters.game_objects -> objects_state -> objects_state
(** [add_object o s] is the state [s] with an added object [o] if tile
    [t] is unoccupied. If tile [t] is occupied, an exception
    OccupiedTile will be raised. Requires that [o] is a valid object. *)

val buy_object :
  Characters.game_objects -> objects_state -> objects_state
(** [buy_object o s] is the state s after an attempt to buy object o has
    been made. Raises [Price] if the state does not have enough
    raindrops to purchase o. *)

val remove_object :
  Characters.game_objects -> objects_state -> objects_state
(** [add_object o s] is the object_state [s] with a removed object [o].
    If object_state [s] does not have [o], an exception will be raised.
    Requires that [o] is a valid object. *)

val state_add_points : int -> objects_state -> objects_state
(** [state_add_points n s] returns a new object state, [s'] such that
    the score of s' is equal to the score in [s] incremented by [n]
    points. *)

val handle_collisions :
  ?spit_only:bool ->
  Characters.game_objects list ->
  objects_state ->
  objects_state
(** [collision s] is state [s] but with fighting collision being
    accounted for when a python and spit are on the same tile, or when
    camls and pythons are on the same tile. A python will disappear if
    its health reaches 0 when spit is on the same tile, a spit will
    disappear when on same tile as python, and a camel will disappear
    when its health reaches 0 when a python is on the same tile. *)
