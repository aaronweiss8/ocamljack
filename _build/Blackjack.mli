(** The type representing the state of the casino game. *)

(** The type representing a casino chip. *)
type chip = Chip.t

(** The type representing a player *)
type player
(** The type representing the different game modes *)
type modes

(** The type representing the category/type of the game *)
type category

(**  [rep_ok t] checks whether the representation of the game state is 
     satisfied *)

module type Mode = sig
  type t
end

(** [players t] is the tuple representing the number of players in the current
    game
*)
val players : t -> int*int

(** [name t] is the name of the room t]*)
val name : t -> string 

(** [bet t b d] places a bet with a list [b] chips and returns the updated
    game state*)
val bet : t -> chip list -> t


