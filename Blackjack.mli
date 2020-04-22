open Game

module type Mode = sig
  type t
  type player
  type deck
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

module Classic : Mode
module CreateGame (M:Mode) : Game


