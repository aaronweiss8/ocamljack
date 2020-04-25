(* The type representing a player. *)
type t

(* [name t] gets the name of player [t] *)
val name : t -> string

(* [get_hand t] is the hand of player [t] *)
val get_hand : t -> Cards.deck

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

(* [bet_chips t bet] moves chips from a users chip stash to their bet *)
val bet_chips : t -> Chip.t -> t

(* [collect_bet t] represents a player [t] that collects their bet chips *)
val collect_bet : t -> t

(* [new_player name chips hand bet bot] creates a new player with a designated
name, amount of chips, and empty hand, an empty bet, and data for whether  *)
val new_player : string -> Chip.t -> Cards.deck -> Chip.t -> bool -> t

(* [update_hand t new_hand] retuns a player with an updated hand *)
val update_hand :  t -> Cards.deck -> t