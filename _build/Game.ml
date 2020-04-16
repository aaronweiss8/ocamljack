(** The type representing a player. *)
type player = Player.t
type t 

type modes

module type Category = sig
end

module Card : Category = struct
  let deal_card deck player = 
    match deck with
    | [] -> failwith "Empty deck!"
    | h::t -> (t * h)

end
module type Game = sig
  val rep_ok : t -> bool
  val rules : t -> 'a list
  val save_name : t -> string
  val players : t -> string
  val player_total : t -> int
  val name : t -> string 
  val bet : t -> chip list -> t
  val score : t -> int
end