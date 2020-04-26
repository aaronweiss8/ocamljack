open Chip

module type Game = sig

  (** The type representing the state of the casino game. *)
  type t
  (** The type representing a casino chip. *)
  type chip 

  (** The type representing a player. *)
  type player

  (**  (**  [rep_ok t] checks whether the representation of the game state is 
        satisfied *)
       val rep_ok : t -> bool

       (** [rules] is a list of the immutable rules of the game *)
       val rules : t -> 'a list

       (** [save_name t] is the name of the file that has contains the save data of the 
       current game*)
       val save_name : t -> string

       (** [players t] is the list of the names of the players in the current game,
       in string format.
   *)
       val players : t -> string

       (** [player_total] t is the number of players in the current game. *)
       val player_total : t -> int

       (** [name t] is the name of the room t]*)
       val name : t -> string 

       (** [bet t b d] places a bet with a chip list [b] and returns the updated
       game state*)
       val bet : t -> player -> chip -> t

       (** [score player] returns the numerical value of chips the player has in the current
       game *)
       val score : player -> int *)

  (** [new_game r] creates a new instance of the game, with a room name r *)
  val new_game : string -> Chip.t -> int -> int -> t

  (** [get_info t] returns a formatted string output detailing the state of the
      game. *)
  val get_info : t -> string

  (** [go t] is the main function responsible for conditionally updating the game state. *)

  val go : t -> Command.command -> t

  (** [create_game pl mb num_decks round] creates a game state with the specified
  playerlist [pl], minimum bet [mb], number of decks [num_decks], and round [round] *)

  val create_game : player list -> int -> int -> int -> t
end
