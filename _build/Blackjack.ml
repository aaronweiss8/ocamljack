open Game
open Cards
open Player

module type Mode = sig
  type player
  type t 
  type deck
  type chip
  (** val deal :  t -> t *)
  (** val hit : t -> player -> deck -> t
      val is_blackjack : t -> bool
      val split : t -> t
      val insurance : t -> player -> t
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

module Classic = struct
  type player = Player.t
  type chip = Chip.t
  type deck = Cards.deck

  (** RI: Specifically for hands, the head of the list is the current player *)
  type t = {round:int;
            players: player list;
            leftMostPlayer: player;
            deck:deck;
            dealer:player}

  let current_player t = t.players |> List.hd

  let get_deck t = t.deck

  let go_next_player game =
    match game.players with
    |p::t -> let new_players = t@[p] in 
      if p = game.leftMostPlayer then
      {round = (game.round + 1);
      players = new_players;
      leftMostPlayer = game.leftMostPlayer;
      deck = game.deck;
      dealer=game.dealer
      }
      else
      {round = (game.round);
      players = new_players;
      leftMostPlayer = game.leftMostPlayer;
      deck = game.deck;
      dealer=gam
      }
    |[] -> failwith "no players" 

  let hit game = 
    let deck = get_deck game in
    match deck with
    | h::r -> (let to_deal = h in
      match game.players with
          |p::b -> let (new_d,new_h) = Cards.transfer_card (deck, Player.get_hand p) to_deal in
            {round = t.round; 
            players = p::b;
            leftMostPlayer = t.leftMostPlayer;
            deck = new_d}
          | [] -> failwith "No players")
    | [] -> failwith "Dealer needs to reset cards"

  let get_info t = "[" ^ "Name : " ^ t.name ^ ", " ^
                   "Round: " ^ (string_of_int t.round) ^ ", " ^ 
                   "Current player: " ^ (current_player t |> Player.name) ^
                   "Last player: " ^ t.leftMostPlayer ^ "]" ^
                   
  (** Change new_game to include initialization of new games
      with decks with varying amounts of cards *)
  let new_game name playername = {name=name;
                       round=0;
                       players=[];
                       leftMostPlayer = (Player.new_player playername Chip.empty 
                       Cards.empty Chip.empty false);
                       deck=Cards.get_standard_deck} 

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

  let get_winners t (ps:player list) =
    let rec check_player

  let split t player = 
    let new_player = {name=t.name;
                        round=next_round t;
                        leftMostPlayer = t.leftMostPlayer;
                        deck=Cards.get_standard_deck}

  let next_round t= 
    match t.current_player with
    | p when p = List.length t.hands -> t.round + 1
    | r -> t.round

  let stand t player = {name=t.name;
                        round=next_round t;
                        leftMostPlayer = t.leftMostPlayer;
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
     leftMostPlayer=t.leftMostPlayer;
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
