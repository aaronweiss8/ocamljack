(* The type representing a player. *)
type t

(* [name t] gets the name of player [t] *)
val name : t -> string

(* [chips t] is a list of the chips that player [t] currently has. *)
val chips : t -> Chip.t 

(* [chips_value t] is the integer value of the chips *)
val chips_value : t -> int

(* [bet t] is a list of the chips that player [t] current is bet *)
val bet : t -> Chip.t

(* [bet_value t] returns the integer value of the bet *)
val bet_value : t -> int

(* [add_chips t c] adds c to the current player t's chips  *)
val add_chips : t -> Chip.t -> t

