open Cards
open Player
open Command

type player = Player.t
type chip = Chip.t
type deck = Cards.deck
exception Cannot_Split
exception Bet_Too_Low
exception Cannot_Perform_Insurance
exception Player_Not_Found

type result =
  | Blackjack
  | Win
  | Tie
  | Push
  | Loss

(** RI: Specifically for players, the head of the list is the current player *)
type t = {round:int;
          min_bet:int;
          players: player list;
          leftMostPlayer: player;
          deck:deck;
          dealer:player}

let current_player t : player = t.players |> List.hd

let get_deck t = t.deck

let get_players t = t.players

let dealer t = t.dealer

let leftMostPlayer t = t.leftMostPlayer

let min_bet t = t.min_bet

let update_playerlst t plst = 
  {round = t.round;
   min_bet = t.min_bet;
   players = plst;
   leftMostPlayer = (List.hd plst);
   deck = t.deck;
   dealer = t.dealer;
  }

let add_player p game = 
  {round = game.round;
   min_bet = game.min_bet;
   players = game.players@[p];
   leftMostPlayer = game.leftMostPlayer;
   deck = game.deck;
   dealer = game.dealer;
  }

let remove_player p game =
  if (List.mem p game.players) && (p <> game.leftMostPlayer) then 
    {round = game.round;
     min_bet = game.min_bet;
     players = (List.filter (fun x -> x = p) game.players);
     leftMostPlayer = game.leftMostPlayer;
     deck = game.deck;
     dealer = game.dealer;}
  else if (List.mem p game.players) && (List.length game.players > 1) then
    let new_plst = (List.filter (fun x -> x = p) game.players) in
    {round = game.round;
     min_bet = game.min_bet;
     players = (new_plst);
     leftMostPlayer = (List.hd new_plst);
     deck = game.deck;
     dealer = game.dealer;}
  else raise Player_Not_Found

let hit game ind d =
  let (newdeck, c) = Cards.deal_one game.deck in
  if d then
    {round = game.round;
     min_bet = game.min_bet;
     players = game.players;
     leftMostPlayer = game.leftMostPlayer;
     deck = newdeck;
     dealer = Player.add_to_hand c 0 game.dealer} 
  else
    match game.players with
    | current::t ->
      if (current = (game.leftMostPlayer)) then
        {round = game.round;
         min_bet = game.min_bet;
         players = (Player.add_to_hand c ind current)::t;
         leftMostPlayer = (Player.add_to_hand c ind current);
         deck = newdeck;
         dealer = game.dealer}
      else
        {round = game.round;
         min_bet = game.min_bet;
         players = (Player.add_to_hand c ind current)::t;
         leftMostPlayer = game.leftMostPlayer;
         deck = newdeck;
         dealer = game.dealer}
    | [] -> failwith "No players"

(**Change to pattern matching from fold *)
let get_hands d p =
  if d then
    match (Player.get_hand p) with
    | [] -> "[Empty]"
    | h::t -> if h = Cards.empty then "[Empty]" else
        match h with
        | a::b -> 
          "<HIDDEN>" ^ "[" ^ 
          (List.fold_left 
             (fun z g -> 
                (Cards.get_rank_string g) ^ " of " ^ 
                (Cards.get_suit_string g)
                ^ ", " ^ z) "]" b)
        | [] -> "[Empty]"

  else 
    p |> get_hand |> function
    | [] -> "[Empty]"
    | hand_list -> if (List.hd hand_list) = Cards.empty then "[Empty]" else
        "[" ^
        (List.fold_left (fun y x -> 
             (List.fold_left 
                (fun z g -> 
                   (Cards.get_rank_string g) ^ " of " ^ 
                   (Cards.get_suit_string g)
                   ^ ", " ^ z) "" x) ^ y) "" ) hand_list
        ^ "]"

let get_chips p = p |> Player.chips |> Chip.to_string

let rec get_bets bets acc =
  match bets with
  | h::t -> get_bets t acc ^ Chip.to_string h
  | [] -> acc

(**Change to printf for alignment *)
let get_info t hide_dealer =
  ANSITerminal.(print_string [blue] ("\nRules:"));
  ANSITerminal.(print_string [default]
                  (" https://bicyclecards.com/how-to-play/blackjack/\n"));
  ANSITerminal.(print_string [blue]  ("Round: "));
  ANSITerminal.(print_string [default] (string_of_int t.round ^ ", "));
  ANSITerminal.(print_string [default] "\nValues:");
  ANSITerminal.(print_string [white] " White = 1;");
  ANSITerminal.(print_string [red] " Red = 5;");
  ANSITerminal.(print_string [blue] " Blue = 10;");
  ANSITerminal.(print_string [green] " Green = 25;");
  ANSITerminal.(print_string [black;Bold] " Black = 100 " );  
  ANSITerminal.(print_string [blue]   ("Minimum Bet: " ^
                                       string_of_int t.min_bet ^ ", "));
  ANSITerminal.(print_string [default]
                  ("\nSimplify your chips means converting "^
                   "your chips to higher denominations."^
                   "\nBreak your chips means converting your "^
                   "chips into lower denominations\n"));
  ANSITerminal.(print_string [yellow] ("Dealer Hand: "));
  ANSITerminal.(print_string [default]
                  ((t.dealer |> get_hands hide_dealer) ^ "\n"));
  ANSITerminal.(print_string [default]
                  ("Leftmost Player: " ^ (Player.name t.leftMostPlayer) ^ "\n" ^  
                   "Current Player: "));
  ANSITerminal.(print_string [green] (current_player t |> Player.name)); 
  ANSITerminal.(print_string [default] "\nPlayer: Hand, Chips, Bet: \n");
  ANSITerminal.(print_string [default] 
                  ((List.fold_left
                      (fun y x -> "\n" ^ Player.name x ^ ": " ^
                                  get_hands false x ^" "^ get_chips x ^ ", " ^
                                  get_bets (Player.bet x) "" ^ ", " ^ y) 
                      " " t.players) ^ "\n"))

(*  ADD ABILITY FOR USER TO CHOOSE ACE VALUE *)
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
let is_blackjack hand = hand_value hand = 21

let did_bust hand = hand_value hand >= 22

(* [split t idx] performs a blackjack "split" of a hand, assuming it can be made
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
                 let new_p = (cp |> Player.add_hand |>
                              Player.add_to_hand (List.nth h 1) (idx + 1) |>
                              Player.add_bet |>
                              Player.bet_chips current_bet (idx + 1) |>
                              Player.remove_from_hand (List.nth h 0) idx ) in
                 let new_player_list np = 
                   match t.players with
                   |h::t -> (np::t) 
                   |[] -> failwith "Cannot Split no player" in
                 let m = if (cp = t.leftMostPlayer) then new_p else
                     t.leftMostPlayer in
                 {round = t.round;
                  min_bet = t.min_bet;
                  players = new_player_list new_p;
                  leftMostPlayer = m;
                  deck = t.deck;
                  dealer = t.dealer}
               else raise Cannot_Split)
  | None -> failwith "Hand Does not exist"

(* [double_down t idx] performs a "double down" on a hand at idx of the current
   player, this also performs the required hit*)
let double_down t idx = 
  let cp = current_player t in
  let current_bet = List.nth (Player.bet cp) idx in
  let new_p = Player.bet_chips current_bet idx cp in
  let new_player_list np = 
    match t.players with
    |h::t -> (np::t) 
    |[] -> failwith "Cannot Double Down on no player" in 
  if (cp = t.leftMostPlayer) then 
    {round = t.round;
     min_bet = t.min_bet;
     players = new_player_list new_p;
     leftMostPlayer = new_p;
     deck = t.deck;
     dealer = t.dealer} 
  else
    let ng = 
      {round = t.round;
       min_bet = t.min_bet;
       players = new_player_list new_p;
       leftMostPlayer = t.leftMostPlayer;
       deck = t.deck;
       dealer = t.dealer} in
    hit ng idx false

let deal_initial_cards game = 

  let rec dic_aux players game_deck accum =
    match players with
    | h::t -> let (new_deck, card) = deal_one game_deck in
      let np = (h |> Player.add_to_hand card 0) in
      dic_aux t new_deck (np::accum)
    | [] -> ((List.rev accum), game_deck) in

  let deal_to_dealer dealer game_deck = 
    let (new_deck, card) = deal_one game_deck in
    let nd = (dealer |> Player.add_to_hand card 0) in
    (nd, new_deck) in

  let new_dealer = (game.dealer |> Player.add_hand) in 

  let (fr_players, gd)= (dic_aux game.players game.deck []) in
  let (fr_dealer, gd2) = (deal_to_dealer new_dealer gd) in
  let (sr_players, gd3) = (dic_aux fr_players gd2 []) in
  let (sr_dealer, gd4) = (deal_to_dealer fr_dealer gd3) in
  {round = game.round;
   min_bet = game.min_bet;
   players = sr_players;
   leftMostPlayer = (List.hd sr_players);
   deck = gd4;
   dealer = sr_dealer;}

(* [next_round t] returns a game after going to the next round*)
let next_round t = 
  let rec collect_and_update player_lst accum = 
    match player_lst with
    |h::t -> let new_p = (h |> Player.collect_bets |> Player.update_hand [])
      in (collect_and_update t (new_p::accum))
    |[] -> List.rev accum in
  let new_player_lst = collect_and_update t.players [] in 
  {round = (t.round + 1);
   min_bet = t.min_bet;
   players = new_player_lst;
   leftMostPlayer = (List.hd new_player_lst);
   deck = t.deck;
   dealer = t.dealer} |> deal_initial_cards

let go_next_player game =
  match game.players with
  |p::t-> let new_players = t@[p] in
    {round = game.round;
     min_bet = game.min_bet;
     players = new_players;
     leftMostPlayer = game.leftMostPlayer;
     deck = game.deck;
     dealer = game.dealer}
  |[] -> failwith "no players" 

(* [place_initial_bets game bets] has all the players place an initial bet above 
   the minimum bet *)
let place_initial_bets game bets =
  let rec pib_aux players bet_lst accum = 
    match (players,bet_lst) with
    |(h::t,b::r) -> 
      if (Chip.get_value b >= game.min_bet) then
        (pib_aux t r ((Player.bet_chips b 0 h)::accum))
      else raise Bet_Too_Low
    |([],[]) -> List.rev accum
    |_ -> failwith "too many bets or too many players" in 
  let new_player_lst = pib_aux game.players bets [] in
  {round = game.round;
   min_bet = game.min_bet;
   players = new_player_lst;
   leftMostPlayer = (List.hd new_player_lst);
   deck = game.deck;
   dealer = game.dealer}

(* [check_hands game] returns a game list with players either winning or
   loosing their bets *)
let check_hands game =

  let dealers_value = hand_value (List.hd (Player.get_hand game.dealer)) in

  let rec ch_hands_aux h_lst accum = 
    match h_lst with
    |h::t -> if (hand_value h < dealers_value) && (dealers_value < 22) then
        ch_hands_aux t (true::accum)
      else ch_hands_aux t ((did_bust h)::accum)
    |[] -> List.rev accum in

  let rec lose_bet_check bool_lst idx player = 
    match bool_lst with
    | (h::t) -> if h 
      then (lose_bet_check t (idx + 1) (Player.lose_bet idx player))
      else
        let hand_val = (hand_value (List.nth (Player.get_hand player) idx)) in
        if (hand_val > dealers_value || dealers_value >= 22) then
          lose_bet_check t (idx + 1) (Player.win_bet idx player)
        else lose_bet_check t (idx + 1) (player)
    | [] -> (Player.collect_bets player) in

  let rec ch_aux players accum =
    match players with
    |h::t -> let bust_lst = ch_hands_aux (Player.get_hand h) [] in
      let np = (lose_bet_check bust_lst 0 h) in
      ch_aux t (np::accum)
    |[] -> List.rev accum in 

  let new_player_lst = ch_aux game.players [] in
  let npl = List.map (fun p -> Player.update_hand [Cards.empty] p)
      new_player_lst in
  let new_dealer = (Player.update_hand [Cards.empty] game.dealer) in

  {round = (game.round + 1);
   min_bet = game.min_bet;
   players = npl;
   leftMostPlayer = (List.hd new_player_lst);
   deck = game.deck;
   dealer = new_dealer}

(* [insurance game player_lst] performs a classic insurance operation with
   the players who want insurance on [game]. [player_lst] is a chip list of the 
   players on the table who want insurance and how much they want.
   Raises: Cannot_Perform_Insurance if a bet is higher than allowed for a 
   side bet or if the dealer is not showing an Ace. *)

let insurance game bets =
  let dealer_top_card = List.nth (List.nth (Player.get_hand game.dealer) 0) 0 in
  let dealer_bottom_card =
    List.nth (List.nth (Player.get_hand game.dealer) 0) 1 in
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
      if is_blackjack (List.nth (Player.get_hand h) 0) then
        let np = (h |> Player.add_chips h_sidebet
                  |> Player.add_chips h_sidebet |> Player.collect_bets) in
        check_player_for_BJ t (np::accum) else
        let np = (h |> Player.add_chips h_sidebet |>
                  Player.add_chips h_sidebet |> Player.lose_bet 1) in 
        check_player_for_BJ t (np::accum)
    |[] -> List.rev accum in

  (* Only called if Dealer does not have BlackJack*)
  let rec remove_side_bets players accum =
    match players with
    |h::t ->
      let np = (h |> Player.lose_bet 1) in 
      check_player_for_BJ t (np::accum)
    |[] -> List.rev accum in

  let make_new_players player_w_sb = 
    if Cards.is_ten dealer_bottom_card then check_player_for_BJ player_w_sb []
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
  players in [pl]*)
let create_game playerlist mb num_decks round =
  let rec starting_deck num accum= 
    if num = 0 then accum |> Cards.combine_decks |> Cards.shuffle 
    else starting_deck (num -1) (Cards.get_standard_deck::accum) in
  let game_dealer = 
    Player.new_player "Dealer" (Chip.create_chips 99999 99999 99999 99999 99999) 
      [Cards.empty] [Chip.empty] true in
  let sd = (starting_deck num_decks []) in
  {round = round;
   min_bet = mb;
   players = playerlist;
   leftMostPlayer = List.hd playerlist;
   deck = sd ;
   dealer = game_dealer;}