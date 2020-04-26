open Game

module type Mode = sig
  type t
  type player
  type deck
  type chip
  val new_game : string -> Chip.t -> int -> int -> t
  val get_info : t -> string
  val go : t -> Command.command -> t
end

module Classic : Mode
module CreateGame (M:Mode) : Game


