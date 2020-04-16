open Chip

type mode

module type Mode = sig
  type t
  val mode : t -> string
end

module type Category = sig
  type t 
end

module type Game = sig

  (** The type representing the state of the casino game. *)
  type t
  (** The type representing a casino chip. *)
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

  (** [players t] is the list of the names of the players in the current game.
  *)
  val players : t -> string

  (** [player_total] t is the number of players in the current game. *)
  val player_total : t -> int

  (** [name t] is the name of the room t]*)
  val name : t -> string 

  (** [bet t b d] places a bet with a list [b] chips and returns the updated
      game state*)
  val bet : t -> chip list -> t

  (** [score] returns the numerical value of chips the player has in the current
      game *)
  val score : t -> int

  (**  *)
end
