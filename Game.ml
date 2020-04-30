module type Category = sig
end

module type Game = sig
  type player 
  type t 
  type chip
  val new_game : string -> Chip.t -> int -> int -> t
  val get_info : t -> string
  (* val go : t -> Command.command -> t *)
  val create_game : player list -> int -> int -> int -> t
end