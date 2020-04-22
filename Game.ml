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
  val rep_ok : t -> bool
  val rules : t -> 'a list
  val save_name : t -> string
  val players : t -> string
  val player_total : t -> int
  val name : t -> string 
  val bet : t -> player -> chip -> t
  val score : player -> int
  val new_game : string -> t
  val get_info : t -> string
end