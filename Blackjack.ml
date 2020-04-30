open Cards
open Player
open Command

type player = Player.t
type chip = Chip.t
type deck = Cards.deck
exception Cannot_Split
exception Bet_Too_Low
exception Cannot_Perform_Insurance

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

let get_info t = failwith "unimplemnted"
(* "[" ^ "Name: Blackjack, " ^
                  "Round: " ^ (string_of_int t.round) ^ ", " ^ 
                  "Current player: " ^ (current_player t |> Player.name) ^ ", " ^
                  "First player: " ^ Player.name t.leftMostPlayer ^ "]" *)

let card_value c =
  match c with
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

(* [check_hands game] returns a game list with players either winning or
   loosing their bets *)
let check_hands game =
  let rec ch_hands_aux h_lst accum = 
    match h_lst with
    |h::t -> ch_hands_aux t ((did_bust h)::accum)
    |[] -> List.rev accum in

  let rec lose_bet_check bool_lst idx player = 
    match (bool_lst) with
    | (h::t) -> if h 
      then  lose_bet_check t (idx + 1) (Player.lose_bet idx player)
      else 
        let bet = List.nth (Player.bet player) idx in
        lose_bet_check t (idx + 1) (Player.add_chips bet player)
    | [] -> Player.collect_bets player in

  let rec ch_aux players accum =
    match players with
    |h::t -> let bust_lst = ch_hands_aux (Player.get_hand h) [] in
      let np = (lose_bet_check bust_lst 0 h) in
      ch_aux t (np::accum)
    |[] -> List.rev accum in 

  {round = game.round;
   min_bet = game.min_bet;
   players = ch_aux game.players [];
   leftMostPlayer = game.leftMostPlayer;
   deck = game.deck;
   dealer = game.dealer}

(* [insurance game player_lst] performs a classic insurance operation with
   the players who want insurance on [game]. [player_lst] is a chip list of the 
   players on the table who want insurance and how much they want.
   Raises: Cannot_Perform_Insurance if a bet is higher than allowed for a 
   side bet or if the dealer is not showing an Ace. *)
let insurance game bets =
  let dealer_top_card = List.nth (List.nth (Player.get_hand game.dealer) 0) 0 in
  let dealer_bottom_card = List.nth (List.nth (Player.get_hand game.dealer) 0) 1 in
  let ace_ex = Cards.make_card (Cards.Heart) (Cards.Red) (Cards.Ace) in

  let rec make_side_bets players bet_lst accum = 
    match (players,bet_lst) with 
    |((p::r),(h::t)) -> let bet_value = Player.bet_value p 0 in
      let side_bet_val = Chip.get_value h in
      if (side_bet_val * 2) <= bet_value then 
        let np = (p |> Player.add_bet |> Player.bet_chips h 1) in
        make_side_bets r t (np::accum)
      else raise Cannot_Perform_Insurance
    |([],[]) -> List.rev accum
    |_ -> failwith "Not every player decided if they are going to bet" in

  (* Only called if Dealer has BlackJack *)
  let rec check_player_for_BJ players accum =
    match players with
    |h::t -> let h_sidebet = List.nth (Player.bet h) 1 in
      if is_blackjack h then
        let np = (h |> Player.add_chips h_sidebet |> Player.add_chips h_sidebet |> 
                  Player.collect_bets) in
        check_player_for_BJ t (np::accum) else
        let np = (h |> Player.add_chips h_sidebet |> Player.add_chips h_sidebet |> 
                  Player.lose_bet 1) in 
        check_player_for_BJ t (np::accum)
    |[] -> List.rev accum in

  (* Only called if Dealer does not have BlackJack*)
  let rec remove_side_bets players accum =
    match players with
    |h::t ->
      let np = (h |> Player.lose_bet 1) in 
      check_player_for_BJ t (np::accum)
    |[] -> List.rev accum in

  let bottom_card_value = (dealer_bottom_card |> Cards.get_rank |> card_value)
  in

  let make_new_players player_w_sb = 
    if bottom_card_value = 10 then check_player_for_BJ player_w_sb []
    else remove_side_bets player_w_sb [] in

  if (Cards.compare dealer_top_card ace_ex) = 0 then
    let new_players1 = make_side_bets game.players bets [] in
    {round = game.round;
     min_bet = game.min_bet;
     players = make_new_players new_players1;
     leftMostPlayer = game.leftMostPlayer;
     deck = game.deck;
     dealer = game.dealer}

  else raise Cannot_Perform_Insurance

(*[create_game pl num_decks r] creates a game state starting at round [r] with 
  players in [pl] FOR TESTING ONLY*)
let create_game playerlist mb num_decks round =
  let rec starting_deck num accum= 
    if num = 0 then accum |> Cards.combine_decks |> Cards.shuffle 
    else starting_deck (num -1) (Cards.get_standard_deck::accum) in
  {round = round;
   min_bet = mb;
   players = playerlist;
   leftMostPlayer = List.hd playerlist;
   deck = starting_deck num_decks [];
   dealer = List.hd playerlist;}

let add_dealer_to_game g p =
  {round=g.round;min_bet=g.min_bet;players=g.players;
   leftMostPlayer=g.leftMostPlayer;deck=g.deck;
   dealer=p}

(* let rec get_command = 
   let inp = read_line () in
   try parse inp
   with
    | Malformed -> (print_string "Malformed\n");
      get_command
    | Empty -> (print_string "Empty\n");get_command *)



(* let do_money r game =
  let do_one_player (b,w,p,l) players =
    match players with
    | h::t -> if List.mem h b then failwith "need blackjack bet 1.5x" else
    if List.mem h w then Player.collect_bets Player.add_chips 
    | [] -> players
  let players_in_order = game.players in
  match r with
  | b::w::p::l::[] -> 
  | _ -> failwith "get_results failed" *)

let get_players t = t.players

let dealer t = t.dealer

let go game (cmd:action) = 
  match cmd with
  | (Hit ind) -> hit game ind
  | (Split ind) -> split game ind
  | (Stand) -> go_next_player game
  | (DD ind) -> double_down game ind
  | (Insurance ind) -> failwith "not implemented"
  | _ -> failwith "should not have gotten here"

(* 
let bot_turn game =
   *)