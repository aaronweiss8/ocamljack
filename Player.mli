(* The type representing a player. *)
type t

(* [name t] gets the name of player [t] *)
val name : t -> string

(* [get_hand t] is the hand of player [t] *)
val get_hand : t -> Cards.deck list

(* [chips t] is a list of the chips that player [t] currently has. *)
val chips : t -> Chip.t 

(* [chips_value t] is the integer value of the chips *)
val chips_value : t -> int

(* [bet t] is a list of the chips that player [t] current is bet *)
val bet : t -> Chip.t list

(* [bet_value t idx] returns the integer value of the bet at the index*)
val bet_value : t -> int -> int

(* [add_chips t c] adds c to the current player t's chips  *)
val add_chips : t -> Chip.t -> t

(* [bet_chips bet idx t] moves chips from a users chip stash 
to their bet at idx *)
val bet_chips : Chip.t -> int -> t -> t

(* [collect_bets t] represents a player [t] that collects their bet chips *)
val collect_bets : t -> t

(* [new_player name chips hand bet bot] creates a new player with a designated
name, amount of chips, and empty hand, an empty bet, and data for whether  *)
val new_player : string -> Chip.t -> Cards.deck -> Chip.t list -> bool -> t

(* [update_hand new_hand t] retuns a player with an updated hand *)
val update_hand : Cards.deck list -> t -> t


(*[add_to_hand c ind t] returns a player with [c] added to the deck at ind *)
val add_to_hand : Cards.card -> int -> t -> t

(* [add_hand t] returns player with an additional hand *)
val add_hand : t -> t

(* [add_bet t] returns a player with an additional bet *)
val add_bet : t -> t