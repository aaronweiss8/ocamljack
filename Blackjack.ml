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
            min_bet:int;
            players: player list;
            leftMostPlayer: player;
            deck:deck;}

  let current_player t = t.players |> List.hd

  let get_deck t = t.deck

  let go_next_player game =
    match game.players with
    |p::t -> let new_players = t@[p] in 
      if p = game.leftMostPlayer then
      {round = (game.round + 1);
      min_bet = game.min_bet;
      players = new_players;
      leftMostPlayer = game.leftMostPlayer;
      deck = game.deck;}
      else
      {round = (game.round);
      min_bet = game.min_bet;
      players = new_players;
      leftMostPlayer = game.leftMostPlayer;
      deck = game.deck;}
    |[] -> failwith "no players" 

  let hit game ind = 
    let (newdeck, c) = Cards.deal_one game.deck in
    match game.players with
    | current::t -> 
            {round = game.round;
            min_bet = game.min_bet;
            players = (Player.add_to_hand current c ind)::t;
            leftMostPlayer = game.leftMostPlayer;
            deck = newdeck;}
    | [] -> failwith "No players"

  let get_info t = "[" ^ "Name : Blackjack, " ^
                   "Round: " ^ (string_of_int t.round) ^ ", " ^ 
                   "Current player: " ^ (current_player t |> Player.name) ^
                   "First player: " ^ Player.name t.leftMostPlayer
                   
  (** Change new_game to include initialization of new games
      with decks with varying amounts of cards *)
  let new_game playername starting_chips mb num_decks = 

    let rec starting_deck num accum= 
      if num = 0 then accum |> Cards.combine_decks |> Cards.shuffle 
      else starting_deck (num -1) (Cards.get_standard_deck::accum) in
    {round=0;
    min_bet = mb;
    players=[];
    leftMostPlayer = (Player.new_player playername starting_chips 
    Cards.empty [Chip.empty] false);
    deck= starting_deck num_decks []} 

  (** Value functions *)
  let card_value hand = function
    | Num x -> x
    | Ace -> 1
    | _ -> 10

  let hand_value phand = 
    let handlist = List.map Cards.get_rank phand in
    let rec sumaces acc num =
      if num = 0 then acc else
      if num = 1 && acc + 11 <= 21 then acc + 11 else
      if 11 + (num-1) + acc <= 21 then acc + 11 + num - 1 else
      acc + num in
    let rec sumhand (hand:Cards.rank list) acc num_aces =
      match hand with
      | [] -> if num_aces = 0 then acc else (sumaces acc num_aces)
      | h::t -> match h with
        | Num x -> (sumhand t (acc + x) num_aces)
        | Ace -> (sumhand t acc (num_aces + 1))
        | _ -> (sumhand t (acc + 10) num_aces) in
    sumhand handlist 0 0

  (**[is_blackjack player] returns true if the current player
      has a blackjack, and false if the current player
      has not 
      currently checks if either of a split hand are blackjack*)
  let is_blackjack player = 
    List.length (Player.get_hand player) = 1 &&
    List.length (List.hd (Player.get_hand player)) = 2 &&
    hand_value (List.hd (Player.get_hand player)) = 21

  (* [get_winners t] returns a list containing a list of players who 
      blackjacked, won against the dealer, and a list of players who
      pushed vs the dealer, then a list of players who lost*)
  let get_results t =
  (* someone should change acc from a list of lists to a tuple of lists *)
    let rec make_win_and_push ps d acc =
      match ps with
      | [] -> acc
      | h::t -> if is_blackjack h then [h::(List.nth acc 0);
      (List.nth acc 1);(List.nth acc 2);(List.nth acc 3)] else
      if (List.exists (fun x -> x) (List.map (fun h -> (d > 21 && hand_value h <= 21) || hand_value h > d && hand_value h <= 21) (Player.get_hand h))) then
      [(List.nth acc 0);h::(List.nth acc 1);(List.nth acc 2);(List.nth acc 3)] else
      if (List.exists (fun x -> x) (List.map (fun h -> hand_value h = d && d <= 21) (Player.get_hand h))) then
      [(List.nth acc 0);(List.nth acc 1);h::(List.nth acc 2);(List.nth acc 3)] else
      [(List.nth acc 0);(List.nth acc 1);(List.nth acc 2);h::(List.nth acc 3)] in
    if is_blackjack t.dealer then
      let pushed = List.fold_left (fun acc x -> if is_blackjack x then x::acc else acc)
      [] t.players in
      [[];[];pushed; List.filter (fun x -> not(List.mem x pushed)) t.players] else
    let d = hand_value (List.hd (Player.get_hand t.dealer)) in
    make_win_and_push t.players d [[];[];[];[]]
    

  (* let split t player = 
    let newhands = Player.get_hand player |> 
    *)
(* 
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
     deck=t.deck} *)

  let score player = Chip.get_value (Player.chips player)

  let name = failwith "unimplemented"

  let player_total t = List.length t.players

  let players t = List.fold_left (fun acc x -> Player.name x ^ " " ^ acc)
                  "" t.players

  let save_name = failwith "unimplemented"

  let rules = failwith "unimplemented"

  let rep_ok = failwith "unimplemented"

  let bet = failwith "unimplemented"
end

(* module Switch : Mode = struct
  include Classic
  let mode = "Switch"
end

module European : Mode = struct
  include Classic
  let mode = "European"
end *)

module CreateGame = 
  functor(M:Mode)->
  struct
    include M
  end
