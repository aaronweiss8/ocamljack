open Chip

(** The type representing the state of the casino game. *)
type t
(** The type representing a casino chip. *))
type chip = Chip.t
(** The type representing the different game modes *)
type modes

(** The type representing the category/type of the game *)
type category

(**  [rep_ok t] checks whether the representation of the game state is 
satisfied *)
val rep_ok : t -> bool

(** [rules] is a list of the immutable rules of the game *)
val rules : t -> 'a list

(** [save_name t] is the name of the file that has contains the save data of the 
current game*)
val save_name : t -> string

(** [players t] is the tuple representing the number of players in the current
game
 *)
val players : t -> int*int

(** [name t] is the name of the room t]*)
val name : t -> string 

(** [bet t b d] places a bet with a list [b] chips and returns the updated
game state*)
val bet : t -> chip list -> t

(**  *)

