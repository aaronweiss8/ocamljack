open Game
open Cards
open Player
open Command

module type Mode = sig
  type player
  type t 
  type deck
  type chip 
  val new_game : string -> Chip.t -> int -> int -> t
  val get_info : t -> string
  val go : t -> Command.command -> t
end

module Classic = struct
  type player = Player.t
  type chip = Chip.t
  type deck = Cards.deck
  exception Cannot_Split
  exception Bet_Too_Low

  (** RI: Specifically for players, the head of the list is the current player *)
  type t = {round:int;
            min_bet:int;
            players: player list;
            leftMostPlayer: player;
            deck:deck;
            dealer:player}

  let current_player t : player = t.players |> List.hd

  let get_deck t = t.deck

  let go_next_player game =
    match game.players with
    |p::t -> let new_players = t@[p] in 
      if p = game.leftMostPlayer then
        {round = (game.round + 1);
         min_bet = game.min_bet;
         players = new_players;
         leftMostPlayer = game.leftMostPlayer;
         deck = game.deck;
         dealer = game.dealer}
      else
        {round = (game.round);
         min_bet = game.min_bet;
         players = new_players;
         leftMostPlayer = game.leftMostPlayer;
         deck = game.deck;
         dealer = game.dealer}
    |[] -> failwith "no players" 

  let hit game ind = 
    let (newdeck, c) = Cards.deal_one game.deck in
    match game.players with
    | current::t -> 
      {round = game.round;
       min_bet = game.min_bet;
       players = (Player.add_to_hand c ind current)::t;
       leftMostPlayer = game.leftMostPlayer;
       deck = newdeck;
       dealer = game.dealer}
    | [] -> failwith "No players"

  let get_info t = "[" ^ "Name: Blackjack, " ^
                   "Round: " ^ (string_of_int t.round) ^ ", " ^ 
                   "Current player: " ^ (current_player t |> Player.name) ^ ", " ^
                   "First player: " ^ Player.name t.leftMostPlayer ^ "]"

  (** Change new_game to include initialization of new games
      with decks with varying amounts of cards *)
  let new_game playername starting_chips mb num_decks = 

    let rec starting_deck num accum= 
      if num = 0 then accum |> Cards.combine_decks |> Cards.shuffle 
      else starting_deck (num -1) (Cards.get_standard_deck::accum) in
    {round=0;
     min_bet = mb;
     players=[(Player.new_player playername starting_chips 
                 Cards.empty [Chip.empty] false)];
     leftMostPlayer = (Player.new_player playername starting_chips 
                         Cards.empty [Chip.empty] false);
     deck= starting_deck num_decks [];
     dealer = (Player.new_player playername starting_chips 
                 Cards.empty [Chip.empty] false);} 

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


  (* [did_bust hand] is true if the specific hand did bust *)  
  let did_bust hand = if hand_value hand < 22 then false else true

  (* [split t idx] performs a blackjack "split" of a hand, assuming it can be made.
     Raises: Cannot_Split if the split cannot be performed *)
  let split t idx = 
    let cp = current_player t in
    let current_bet = List.nth (Player.bet cp) idx in
    let hand_lst = (cp |> Player.get_hand) in
    let hand = (List.nth_opt hand_lst idx) in
    match hand with
    | Some h -> (if (List.length h = 2 && 
                     let c1 = List.nth h 0 in 
                     let c2 = List.nth h 1 in
                     Cards.compare c1 c2 = 0) then
                   let new_p = (cp |> Player.add_hand |> Player.add_to_hand (List.nth h 0) (idx + 1) |>
                                Player.add_bet |> Player.bet_chips current_bet (idx + 1)) in
                   let new_player_list np = 
                     match t.players with
                     |h::t -> (np::t) 
                     |[] -> failwith "Cannot Split no player" in 
                   {round = t.round;
                    min_bet = t.min_bet;
                    players = new_player_list new_p;
                    leftMostPlayer = t.leftMostPlayer;
                    deck = t.deck;
                    dealer = t.dealer}
                 else raise Cannot_Split)
    | None -> failwith "Hand Does not exist"

  (* [double_down t idx] performs a "double down" on a hand at idx of the current player,
     this also performs the required hit*)
  let double_down t idx = 
    let cp = current_player t in
    let current_bet = List.nth (Player.bet cp) idx in
    let new_p = Player.bet_chips current_bet idx cp in
    let new_player_list np = 
      match t.players with
      |h::t -> (np::t) 
      |[] -> failwith "Cannot Double Down on no player" in 
    let ng = 
      {round = t.round;
       min_bet = t.min_bet;
       players = new_player_list new_p;
       leftMostPlayer = t.leftMostPlayer;
       deck = t.deck;
       dealer = t.dealer} in
    hit ng idx

  (* [next_round t] returns a game after going to the next round *)
  let next_round t = 
    let rec collect_and_update player_lst accum = 
      match player_lst with
      |h::t -> let new_p = (h |> Player.collect_bets |> Player.update_hand [])
        in collect_and_update t (new_p::accum)
      |[] -> List.rev accum in
    {round = (t.round + 1);
     min_bet = t.min_bet;
     players = collect_and_update t.players [];
     leftMostPlayer = t.leftMostPlayer;
     deck = t.deck;
     dealer = t.dealer}

  (* [place_initial_bets game bets] has all the players place an initial bet above 
     the minimum bet *)
  let place_initial_bets game bets =
    let rec pib_aux players bet_lst accum = 
      match (players,bet_lst) with
      |(h::t,b::r) -> if Chip.get_value b >= game.min_bet then
          pib_aux t r ((Player.bet_chips b 0 h)::accum)
        else raise Bet_Too_Low
      |([],[]) -> List.rev accum
      |_ -> failwith "too many bets or too many players" in 
    {round = game.round;
     min_bet = game.min_bet;
     players = pib_aux game.players bets [];
     leftMostPlayer = game.leftMostPlayer;
     deck = game.deck;
     dealer = game.dealer}

  (*[check_hands game] returns a game list with players either winning or
    loosing their bets
    let check_hands game =
    let rec ch_hands_aux h_lst accum = 
      match h_lst with
      |h::t -> ch_hands_aux t ((did_bust h)::accum)
      |[] -> List.rev accum in

    let rec ch_aux players accum =
      match players with
      |h::t -> let bust_lst = ch_hands_aux (Player.get_hand h) [] in

      |[] -> List.rev accum *)

  let go game (cmd:command) = 
    match cmd with
    | Command (Hit ind) -> print_endline "Terrible"; hit game ind
    | Command (Split ind) -> split game ind
    | Command (Stand) -> go_next_player game
    | Empty | Malformed-> print_endline "Invalid command!"; game
    | Command (DD ind) -> double_down game ind
    | Command (Insurance ind) -> failwith "not implemented"

end
(*
module Switch : Mode = struct
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
