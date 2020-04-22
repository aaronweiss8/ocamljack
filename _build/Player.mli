(* The type representing a player. *)
type t

(* [name t] gets the name of player [t] *)
val name : t -> string

(* [chips t] returns a list of the chips that player [t] currently has. *)
val chips : t -> Chip.t 

val score : t -> int

val update_chips : t -> Chip.t -> t

