(* characters.mli *)
(** Representation of the defensive types of the game, that is the camls
    (which shoot) and the cacti (which collect raindrops) *)

type spit
(** The abstract type of values representing an a camel spit attack. *)

type caml
(** The abstract type of values representing a camel. *)

type cactus
(** The abstract type of values representing a cactus. *)

type python
(** The abstract type of values representing a python. *)

type character
(** The abstract type of values representing either a caml, cactus, or
    python, for the case of fields shared by all three characters. *)

type game_objects
(** The abstract type that represents both characters and spits. *)

val object_string : game_objects -> string
(** [object_string o] A string short-hand representation of a single
    object [o] would look like on the board. A spit looks like "*"; A
    Camel looks like "Caml"; A Python looks like "Py"; a Cactus looks
    like "Cact". *)

val print_object : game_objects -> unit
(**[print_object o] prints the object o's string representation using
   its designated ANSI color*)

val are_enemies : game_objects -> game_objects -> bool
(** [are_enemies o1 o2] is true if two objects [o1] and [o2] are enemies
    and false if they aren't enemies. Objects are enemies if o1 and o2
    are a python-spit pair, python-cactus pair, or a caml-python pair.
    The order of [o1] and [o2] does not affect the result. *)

val init_caml : ?health:int -> int -> int -> int -> game_objects
(** [init_caml h x y ] is a new caml with [h] number of humps and at the
    horizontal position of [x] to the right and vertical position [y]
    down on a board. *)

val init_cactus : ?health:int -> int -> int -> game_objects
(** [init_cactus x y] is a new cactus at the horizontal position of [x]
    to the right and vertical position [y] down on a board. *)

val init_python : ?health:int -> int -> int -> game_objects
(** [init_python x y] is a new python at the horizontal position of [x]
    to the right and vertical position [y] down on a board. *)

val init_spit : int -> int -> game_objects
(** [init_spit x y] is a new spit at the horizontal position of [x] to
    the right and vertical position [y] down on a board. *)

val compare_game_objects :
  ?ignore_health:bool -> game_objects -> game_objects -> bool
(** [compare_game_objects o1 o2] is true if game object [o1] is the same
    as game object [o2]. Is false if [o1] is not the same game object as
    [o2]. If optional [ignore_health] arguement not applied, defaults to
    false. If [ignore_health] is false, health is not taken into account
    when comparing [o1] and [o1]. Else if true, health is taken into
    account when comparing. *)

val can_spit : game_objects -> bool
(** [can_spit o] returns whether object o can spit. Always returns false
    for objects that are not camels*)

val incr_spit_timer : game_objects -> game_objects
(** [incre_spit_timer o] is object o with an updated spit timer by
    removing 1 from the timer field of [o] (wraps around with speed of
    object for values less than 0). If [o] is not of Caml type, then no
    change occurs.*)

val get_rain_rate : game_objects -> int
(** [get_rain_rate o] is the rain_rate of the cactus object. The output
    does not depend on whether the game_object is dead or not. 0 if not
    a cactus.*)

val get_object_points : game_objects -> int
(** [get_points_object o] are the points that object (Python) is worth.
    0 if not python.*)

val get_health : game_objects -> int
(** [get health c] is the numerical value of health character [c]. 0
    means character is dead. *)

val get_object_damage : game_objects -> int
(** [get_object_damage c] is the damage that game_object [c] has on any
    other game_object. Since caml and cactus objects don't damage other,
    they have a damage value of 0. *)

val damage_character : game_objects -> int -> game_objects option
(** [damage_character o amount] returns Some of an alive object or None
    after deducting amount from its health. *)

val get_cost : game_objects -> int
(** [get cost c] is the numerical cost to place character [c] on a
    board. *)

val get_obj_position : game_objects -> int * int
(** [get_obj_position o] is the tuple position of game_object [o] where
    the first int of the tuple is the horizontal position of the
    character and the second int of the tuple is the vertical position. *)

val get_timer : game_objects -> int
(** [get_timer o] is the int representation of the spit timer of
    game_object [o]. When [o] is 0, the Caml spits, otherwise [o]
    represents the time before the Caml will spit. Because only Camls
    have a timer field, any other game object will just return a time of
    0.*)

val update_pos : int * int -> game_objects -> game_objects
(** [update_pos p c] is character [c] but it's location is now at p. *)
