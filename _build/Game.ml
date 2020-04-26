module type Category = sig
end

module Card : Category = struct
end

module Dice : Category = struct
end

module type Game = sig
  type player 
  type t 
  type chip
  val new_game : string -> Chip.t -> int -> int -> t
  val get_info : t -> string
  val go : t -> Command.command -> t
end