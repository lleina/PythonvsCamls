(* characters.ml *)

let print_red = ANSITerminal.print_string [ ANSITerminal.red ]
let print_green = ANSITerminal.print_string [ ANSITerminal.green ]
let print_yellow = ANSITerminal.print_string [ ANSITerminal.yellow ]
let print_magenta = ANSITerminal.print_string [ ANSITerminal.magenta ]
let print_cyan = ANSITerminal.print_string [ ANSITerminal.cyan ]

type spit = {
  damage : int;
  speed : float;
  position : int * int;
}

type caml = {
  health : int;
  num_humps : int;
  cost : int;
  spit_type : spit;
  position : int * int;
  timer : int;
  speed : int;
}

type cactus = {
  health : int;
  cost : int;
  rain_rate : int;
  position : int * int;
}

type python = {
  health : int;
  speed : float;
  position : int * int;
  damage : int;
  points : int;
}

type character =
  | Caml of caml
  | Cactus of cactus
  | Python of python

type game_objects =
  | Ch of character
  | Sp of spit

let object_string = function
  | Sp _ -> "*"
  | Ch character -> (
      match character with
      | Caml _ -> "Caml"
      | Python _ -> "Py"
      | Cactus _ -> "Cact")

(*💧🐪🌵🐍*)
let print_object o =
  let o_str = object_string o in
  match o with
  | Sp _ -> print_cyan o_str
  | Ch (Caml _) -> print_yellow o_str
  | Ch (Cactus _) -> print_green o_str
  | Ch (Python _) -> print_red o_str

let are_enemies o1 o2 =
  match o1 with
  | Sp _
  | Ch (Caml _)
  | Ch (Cactus _) ->
      object_string o2 == "Py"
  | Ch (Python _) -> object_string o2 != "Py"

let init_caml ?(health = 1) number_of_humps pos_x pos_y =
  Ch
    (Caml
       {
         health;
         cost = 100;
         spit_type =
           (if number_of_humps = 1 then
            { speed = 1.; damage = 2; position = (pos_x, pos_y) }
           else { speed = 1.; damage = 2; position = (pos_x, pos_y) });
         num_humps = number_of_humps;
         position = (pos_x, pos_y);
         timer = 0;
         speed = 2;
       })

let init_cactus ?(health = 10) pos_x pos_y =
  Ch
    (Cactus
       { health; cost = 50; position = (pos_x, pos_y); rain_rate = 10 })

let init_python ?(health = 2) pos_x pos_y =
  Ch
    (Python
       {
         health;
         speed = 2.;
         position = (pos_x, pos_y);
         damage = 2;
         points = 100;
       })

let init_spit pos_x pos_y =
  Sp { damage = 1; speed = 1.; position = (pos_x, pos_y) }

(* helper function *)
let compare_position (pos_x1, pos_y1) (pos_x2, pos_y2) =
  pos_x1 = pos_x2 && pos_y1 = pos_y2
(* end helper function *)

let compare_game_objects ?(ignore_health = false) o1 o2 =
  match (o1, o2) with
  | ( Sp { damage = str1; speed = spd1; position = pos1 },
      Sp { damage = str2; speed = spd2; position = pos2 } ) ->
      str1 = str2 && spd1 = spd2 && compare_position pos1 pos2
  | ( Ch (Caml { health = h1; num_humps = nh1; position = pos1 }),
      Ch (Caml { health = h2; num_humps = nh2; position = pos2 }) ) ->
      (h1 = h2 || ignore_health)
      && nh1 = nh2
      && compare_position pos1 pos2
  | ( Ch (Cactus { health = h1; rain_rate = rr1; position = pos1 }),
      Ch (Cactus { health = h2; rain_rate = rr2; position = pos2 }) ) ->
      (h1 = h2 || ignore_health)
      && rr1 = rr2
      && compare_position pos1 pos2
  | ( Ch (Python { health = h1; speed = s1; position = pos1 }),
      Ch (Python { health = h2; speed = s2; position = pos2 }) ) ->
      (h1 = h2 || ignore_health)
      && s1 = s2
      && compare_position pos1 pos2
  | _ -> false

let get_health = function
  | Ch (Caml { health })
  | Ch (Cactus { health })
  | Ch (Python { health }) ->
      health
  | _ -> 0

(* helper function *)
let set_health o num =
  match o with
  | Ch (Caml x) -> Ch (Caml { x with health = num })
  | Ch (Cactus x) -> Ch (Cactus { x with health = num })
  | Ch (Python x) -> Ch (Python { x with health = num })
  | x -> x
(* helper function *)

let get_object_damage = function
  | Ch (Python p) -> p.damage
  | Sp s -> s.damage
  | _ -> 0

let damage_character o i =
  let health = get_health o in

  if health - i <= 0 then None else Some (set_health o (health - i))

let get_object_points = function
  | Ch (Python { points }) -> points
  | _ -> 0

let get_cost = function
  | Ch (Caml { cost })
  | Ch (Cactus { cost }) ->
      cost
  | _ -> 0

let get_obj_position = function
  | Sp { position }
  | Ch (Caml { position })
  | Ch (Cactus { position })
  | Ch (Python { position }) ->
      position

let get_rain_rate = function
  | Ch (Cactus { rain_rate }) -> rain_rate
  | _ -> 0

let can_spit = function
  | Ch (Caml c) -> c.timer < 1
  | _ -> false

let incr_spit_timer = function
  | Ch (Caml c) ->
      Ch
        (Caml
           {
             c with
             timer = (if c.timer < 1 then c.speed else c.timer - 1);
           })
  | o -> o

let get_timer = function
  | Sp { position; _ } -> 0
  | Ch c -> (
      match c with
      | Caml { timer; _ } -> timer
      | _ -> 0)

let update_pos (pos : int * int) (ch : game_objects) =
  match ch with
  | Ch (Caml x) -> Ch (Caml { x with position = pos })
  | Ch (Cactus x) -> Ch (Cactus { x with position = pos })
  | Ch (Python x) -> Ch (Python { x with position = pos })
  | Sp x -> Sp { x with position = pos }
