open Game
open Cards

module type Mode = sig
  type player = Player.t
  type t 
  type deck
  (** val deal :  t -> t *)
  val hit : t -> player -> deck -> t
  val is_blackjack : t -> bool
  (** 
     val split : t -> t
     val insurance : t -> player -> t
  *)
  val stand : t -> player -> t
  val mode : string
end

module Classic : Mode = struct
  type hand = Cards.card 
  type player = Player.t
  type deck = Cards.card list
  type t = {name:string;round:int;
            hands:(player * hand) list;
            current_player:int;
            deck:deck}
  let get_deck t = t.deck
  let hit t player = 
    failwith "Unimplemented"
  (** 
     t.hands |> List.map (fun (x,y) -> 
        if (x=player) then (x, player::hand)) *)
  let is_blackjack player = 
    failwith "Unimplemented"
  let current_player t = t.current_player 
                         |> List.nth (t.hands |> List.split |> fst)
  let new_game name = {name=name;round=0;hands=[];current_player=0}
  let stand t player = failwith "Unimplemented"
  let mode = ""
end

module Switch : Mode = struct
  include Classic
end

module European : Mode = struct
  include Classic
end

module CreateGame = 
  functor(M:Mode)->functor(C:Category)->
  struct
    include M
    include C
    type chip = Chip.t
  end
