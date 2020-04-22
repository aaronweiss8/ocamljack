open Game
open Cards

module type Mode = sig
  type player
  type t 
  type deck
  type chip
  (** val deal :  t -> t *)
  (** val hit : t -> player -> deck -> t
      val is_blackjack : t -> bool
      (** 
      val split : t -> t
      val insurance : t -> player -> t
   *)
      val stand : t -> player -> t
      val mode : string *)
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

module Classic : Mode = struct
  type hand = Cards.card list option
  type player = Player.t
  type chip = Chip.t
  type deck = Cards.card list
  type t = {name:string;round:int;
            hands:(player * hand) list;
            current_player:int;
            deck:deck}

  let current_player t = t.current_player 
                         |> List.nth (t.hands |> List.split |> fst)
  let get_deck t = t.deck
  let hit t = failwith "Unimplemented"
  (*
    current_player t|> y
     t.hands |> List.map (fun (x,y) -> 
        if (x=player) then (x, player::hand)) *)

  let get_info t = "[" ^ t.name ^ ", " ^
                   (string_of_int t.round) ^ ", " ^ 
                   (string_of_int t.current_player) ^ "]"

  (** Change new_game to include initialization of new games
      with decks with varying amounts of cards *)
  let new_game name = {name=name;
                       round=0;
                       hands=[];
                       current_player=0;
                       deck=Cards.get_standard_deck}

  (** Helper update functions *)
  let next_player t =
    match t.current_player with
    | p when p = List.length t.hands -> 0
    | p -> p + 1

  (** Value functions *)
  let card_value = function
    | Num x -> x
    | Ace -> 1
    | _ -> 10

  let hand_value t player = 
    let handlist = t.hands |> 
                   List.assoc player |> 
                   Option.get |> 
                   List.map Cards.get_rank |> 
                   List.map card_value in
    List.fold_right (fun x y -> x + y) handlist 0

  (**[is_blackjack t] returns true if the current player
      has reached blackjack, and false if the current player
      has not *)
  let is_blackjack t player = 
    match hand_value t player with
    | 21 -> true
    | _ -> false

  let next_round t= 
    match t.current_player with
    | p when p = List.length t.hands -> t.round + 1
    | r -> t.round

  let stand t player = {name=t.name;
                        round=next_round t;
                        hands=[];
                        current_player=next_player t;
                        deck=Cards.get_standard_deck}
  let mode = "Classic"
  let rules t = failwith "Unimplemented"
  let rep_ok t = failwith "Unimplemmented"
  let save_name t = failwith  "Unimplemented"
  let player_list t = t.hands |> List.split |> fst
  let players t = (player_list t 
                   |> List.fold_right (fun x y-> Player.name x ^ ", " ^ y)) ""
  let player_total t = List.length t.hands
  let name t = t.name
  let bet t player chip = 
    let new_chips = Chip.bet chip (Player.chips player) in
    let newplayers = t.hands |> 
                     List.map (fun (x,y) -> if x = player then
                                  (Player.update_chips x new_chips,y) 
                                else (x,y)) in
    {name=t.name;
     round=t.round;
     hands=newplayers;
     current_player=t.current_player;
     deck=t.deck}

  let score player = Chip.get_value (Player.chips player)
end

module Switch : Mode = struct
  include Classic
  let mode = "Switch"
end

module European : Mode = struct
  include Classic
  let mode = "European"
end

module CreateGame = 
  functor(M:Mode)->
  struct
    include M
  end
