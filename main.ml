
open Blackjack
open Player
open Command
open Chip

(**[simp_or_break players accum] prompts each user to simplify or break their
   chips, and does so with player methods*)
let rec simp_or_break players accum =
  let sob_aux player is_simp steps = 
    if is_simp then (Player.simplify_chips steps player) 
    else (Player.break_chips steps player) in

  match players with
  | h::t -> 
    (ANSITerminal.(print_string [yellow] ("\n" ^ (Player.name h));
                   ANSITerminal.(print_string [default]
                                   (", do you" ^ " want to simplify or break "^
                                    "your chips and how many times each?" ^
                                    "\nType Break or Simplify or Pass\n |> ")));
    let input = if (Player.is_user h) then (read_line ()) else "Pass" in
     match input with
     | "Break" -> 
       ANSITerminal.(print_string [blue] "How many steps? \n|> "); 
       let steps = (read_int ()) in
       simp_or_break t ((sob_aux h false steps)::accum)
     | "Simplify" ->
       ANSITerminal.(print_string [blue] "How many steps? \n|> ");
       let steps = (read_int ()) in
       simp_or_break t ((sob_aux h true steps)::accum)
     | "Pass" -> simp_or_break t (h::accum)
     | _ -> ANSITerminal.(print_string [red] "\nMalformed input, try again");
       simp_or_break players accum)
  | [] -> List.rev accum

(** [remove p a] asks each player if they want to leave the game and takes them
    out if they say yes*)
let rec remove players accum =
  match players with
  |h::t -> 
    let name = Player.name h in
    ANSITerminal.(print_string [red] name);
    ANSITerminal.(print_string [default]
                    (", do you want to leave the game? (y/_ \n |>"));
    (match (read_line ()) with
     | "y" -> ANSITerminal.(print_string [yellow] ("Goodbye " ^ name ^ "!\n"));
       remove t (accum)
     | _ ->
       remove t (h::accum))
  |[] -> List.rev accum

(** [get_num s] returns int of string s unless s is not an int,
    then it tries again *)
let rec get_num s =
  try int_of_string s with
  | Failure x -> ANSITerminal.(print_string [red] "Not a number. Try again: ");
    get_num (read_line ())

(**[get_chips ()] prompts a user to make a set of chips for whatever the
   specific case is. *)
let get_chips () = 
  (ANSITerminal.(print_string [default] ("\nEnter the number of "));
   ANSITerminal.(print_string [white] "white chips: ");
   let w = get_num (read_line ()) in
   ANSITerminal.(print_string [default] ("Enter the number of "));
   ANSITerminal.(print_string [red] "red chips: ");
   let r = get_num (read_line ()) in
   ANSITerminal.(print_string [default] ("Enter the number of "));
   ANSITerminal.(print_string [blue] "blue chips: ");
   let b = get_num (read_line ()) in
   ANSITerminal.(print_string [default] ("Enter the number of "));
   ANSITerminal.(print_string [green] " green chips: ");
   let g = get_num (read_line ()) in
   ANSITerminal.(print_string [default] ("Enter the number of "));
   ANSITerminal.(print_string [black;Bold] " black chips: ");
   let bla = get_num (read_line ()) in
   Chip.create_chips w r b g bla)

(*[get_pre_round_bet players accum] returns a chip list of all the players
  to-bet bets *)
let rec get_pre_round_bet players min_bet accum =
  match players with
  | h::t -> ANSITerminal.(print_string [blue]
                            ("\n" ^ (Player.name h) ^ "'s bet! \n"));
    if Player.is_user h then
      let to_bet = get_chips () in
      if Chip.get_value to_bet < min_bet then 
        (ANSITerminal.(print_string [red]
                         "This was less than the minimum bet! Try again\n");
         get_pre_round_bet (h::t) min_bet accum) 
      else if (Chip.is_within (Player.chips h) to_bet) then
        get_pre_round_bet t min_bet (to_bet::accum) else
        (ANSITerminal.(print_string [red] "Too many chips bet. Try again.\n");
         get_pre_round_bet (h::t) min_bet accum)
    else
      get_pre_round_bet t min_bet ((create_chips 0 3 0 0 0)::accum)
  | [] -> List.rev accum

(** [dealer_turn g] returns a game after the dealer has finished their turn*)
let rec dealer_turn game =
  let d_hand = game |> Blackjack.dealer |> Player.get_hand |> List.hd in
  if Blackjack.hand_value d_hand < 17 then
    (dealer_turn (Blackjack.hit game 0 true))
  else game

(** [check_players_have_mula g] allows the game to continue if each player
    has enough money to make the minimum bet*)
let check_players_have_mula game =
  let players = Blackjack.get_players game in
  let min_bet = Blackjack.min_bet game in
  let rec cphm_aux p_lst mb accum =
    match p_lst with
    | h::t -> 
      (let cp_chips = Player.chips_value h in
       if cp_chips >= min_bet then
         cphm_aux t mb (h::accum)
       else 
         (ANSITerminal.(print_string [red]( (Player.name h)
                                            ^ ", sorry you need more money."));
          cphm_aux t mb accum))
    | [] -> List.rev accum in
  let new_players = cphm_aux players min_bet [] in
  Blackjack.update_playerlst game new_players

(**[play_and_rotate game ind on_dealer] does the "Heavy Lifting" for the 
   functionality of the game *)
let rec play_and_rotate game ind on_dealer =

  ANSITerminal.(print_string [green;Bold]
                  "\n=================================\n");
  if on_dealer then 
    let ng = dealer_turn game in
    ANSITerminal.(print_string [green] "\nDealer's Turn!\n");
    Blackjack.get_info ng false;
    ANSITerminal.(print_string [yellow;Bold]
                    "\n===============Dealing is over===============\n");
    ng
  else

    let cp = Blackjack.current_player game in
    let player_hand = (cp |> get_hand |> List.nth) ind in
    let dealer_hand = Blackjack.dealer game |> get_hand |> List.hd in
    ANSITerminal.(print_string [red;Bold]
                    ("\nRecommended Move: " ^ 
                     Cards.recommendation player_hand dealer_hand ^ "\n"));
    ANSITerminal.(print_string [magenta;Bold]
                    ("\n" ^ (Player.name cp) ^ ",what is your move?\n" ^ 
                     "The command options are hit, double down, stand,"^
                     " quit, split, or insurance. \n|> "));
    let input  = if (Player.is_user cp) then (read_line ())
    else Cards.recommendation player_hand dealer_hand in
      match (parse input ind) with
      | Hit -> hit game ind on_dealer
      | Stand -> stand cp game ind on_dealer
      | DD -> double_down game ind on_dealer
      | Split -> split game ind on_dealer
      | Insurance -> insurance game ind on_dealer
      | Quit -> ANSITerminal.(print_string [yellow] "Goodbye!"); exit 0

(** [hit g i o] is a mutually recursive function that performs the hit action
    then calls play and rotate on the game after the hit*)
and hit game ind on_dealer =
  let ng = Blackjack.hit game ind on_dealer in
  let new_cp = Blackjack.current_player ng in
  let hand_val = Blackjack.hand_value
      (List.nth (Player.get_hand new_cp) ind) in
  ANSITerminal.(print_string [cyan]
                  ("\nHand is worth " ^ (string_of_int hand_val) ^
                   " after hitting"));
  Blackjack.get_info ng true;
  let num_hands_of_cp = (new_cp |> Player.get_hand |> List.length) in
  if(hand_val > 21) && (ind = (num_hands_of_cp - 1)) then
    let rotated_game = (Blackjack.go_next_player ng) in
    if (Blackjack.current_player rotated_game =
        Blackjack.leftMostPlayer rotated_game)
    then play_and_rotate rotated_game 0 true
    else play_and_rotate rotated_game 0 false
  else if (hand_val > 21) then
    play_and_rotate ng (ind + 1) on_dealer
  else play_and_rotate ng ind on_dealer

(** [stand g i o] is a mutually recursive function that performs the
    stand action, moving on to the next player
    then calls play and rotate on the game after the hit*)
and stand cp game ind on_dealer =
  let hand_val = Blackjack.hand_value (List.nth (Player.get_hand cp) ind) in
  ANSITerminal.(print_string [cyan] ("\nYou chose to stand on " ^
                                     (string_of_int hand_val)));
  Blackjack.get_info game true;
  let num_hands_of_cp = (cp |> Player.get_hand |> List.length) in
  if (num_hands_of_cp - 1) = ind then
    let rotated_game = (Blackjack.go_next_player game) in
    if (Blackjack.current_player rotated_game =
        Blackjack.leftMostPlayer rotated_game)
    then play_and_rotate rotated_game 0 true
    else play_and_rotate rotated_game 0 false
  else play_and_rotate game (ind + 1) on_dealer

(** [double_down g i o] is a mutually recursive function that performs the
    double down action, and subseuently plays and rotates on the game state. *)
and double_down game ind on_dealer =
  try (let ng = Blackjack.double_down game ind in
  let new_cp = Blackjack.current_player ng in
  let hand_val = Blackjack.hand_value
      (List.nth (Player.get_hand new_cp) ind) in
  ANSITerminal.(print_string [cyan]
                  ("\nHand is worth " ^ (string_of_int hand_val) ^
                   " after doubling down"));
  Blackjack.get_info ng true;
  let num_hands_of_cp = (new_cp |> Player.get_hand |> List.length) in 
  if(ind = (num_hands_of_cp - 1)) then
    let rotated_game = (Blackjack.go_next_player ng) in
    if (Blackjack.current_player rotated_game =
        Blackjack.leftMostPlayer rotated_game)
    then play_and_rotate rotated_game 0 true
    else play_and_rotate rotated_game 0 false
  else play_and_rotate ng (ind + 1) on_dealer )
  with
  | Chip.Not_Within -> ANSITerminal.(print_string [red] "\nYou cannot split, you
   need more chips\n");
    play_and_rotate game ind on_dealer

(** [split g i o] is a mutually recursive function that performs the split
    action, and subseuently plays and rotates on the game state. *)
and split game ind on_dealer =
  try (
    let ng = Blackjack.split game ind in
    ANSITerminal.(print_string [blue] "\nYou split your cards.\n");
    Blackjack.get_info ng true;
    play_and_rotate ng ind on_dealer
  )
  with
  | Cannot_Split -> ANSITerminal.(print_string [red] "\nYou cannot split\n");
    play_and_rotate game ind on_dealer
  | Chip.Not_Within -> ANSITerminal.(print_string [red] "\nYou cannot split, you
   need more chips\n");
    play_and_rotate game ind on_dealer

(** [insurance g i o] is a mutually recursive function that performs the
    insurance action, and subseuently plays and rotates the game state. *)
and insurance game ind on_dealer =
  if Cards.get_rank (List.nth (List.nth (game|>Blackjack.dealer|>Player.get_hand) 1) 1)
    = Cards.Ace then
  try (
    let side_bets = get_pre_round_bet (Blackjack.get_players game) 0 [] in
    let ng = Blackjack.insurance game side_bets in
    ANSITerminal.(print_string [blue] "\nYou put insurance on your cards\n");
    play_and_rotate ng ind on_dealer
  )
  with
  | Cannot_Perform_Insurance -> ANSITerminal.(print_string [red]
                                                "\nCannot insure hand.\n");
    play_and_rotate game ind on_dealer
  else 
    (ANSITerminal.(print_string [red] "\nCannot insure hand.\n");
    play_and_rotate game ind on_dealer)
    

(** [play g] is a function for the main play loop of the game, which outputs
    a game specified by user input *)
let rec play game =

  if List.length (Blackjack.get_players game) = 0 then
  (ANSITerminal.(print_string [red] "\nEveryone left!\n");
  exit 0;)
  else

  (** [do_beginning g] does all beginning checks and input up to dealing cards*)
  let do_beginning game =
    let check_mula = check_players_have_mula game in
    let simp_break_ed_game = Blackjack.update_playerlst check_mula
        (simp_or_break (Blackjack.get_players check_mula) []) in
    (*AT THIS POINT THE USER HAS THE OPTION TO BREAK OR SIMPLIFY THEIR CHIPS *)

    Blackjack.get_info simp_break_ed_game true;
    ANSITerminal.(print_string [magenta]
                    "Place initial bets, remember the minimum bet!");
    let bets = get_pre_round_bet (Blackjack.get_players simp_break_ed_game)
        (Blackjack.min_bet simp_break_ed_game) [] in
    let betted_game = Blackjack.place_initial_bets simp_break_ed_game bets in
    (*AT THIS POINT THE USER HAS PLACED THEIR INITIAL BET *)
    Blackjack.get_info betted_game true;
    ANSITerminal.(print_string [red;Bold] ("\n============================\n" ^
                                           "Cards have been dealt" ^
                                           "\n============================\n")); 
    (Blackjack.deal_initial_cards betted_game) in

  ANSITerminal.(print_string [green] ("\nDo you want to add another player "^
                                      "to the table, remove a player, or "^
                                      "neither (if neither hit any key besides"^
                                      "y and r)? (y/r/n)\n>"));
  try (match (read_line ()) with
      | "y" -> add_new_player game
      | "r" -> let new_plst = remove (Blackjack.get_players game) [] in
        let ng = Blackjack.update_playerlst game new_plst in play ng
      | _ ->
        let game_after_delt = do_beginning game in
        (*AT THIS POINT THE USER HAS BEEN DELT THEIR INITIAL CARDS *)
        Blackjack.get_info game_after_delt true;   
        let play_round = play_and_rotate game_after_delt 0 false in 
        (*AT THIS POINT THE ROUND HAS BEEN PLAYED *)
        let round_with_check = Blackjack.check_hands play_round in
        (*AT THIS POINT THE BETS HAVE BEEN COLLECTED OR DISTRIBUTED *)
        (* let next_round = Blackjack.next_round round_with_check in
           Blackjack.get_info next_round; *)
        Blackjack.get_info round_with_check false;
        ANSITerminal.(print_string [red;Bold] "\n=======Round over=========\n");
        play round_with_check)
  with
  | Failure m -> (ANSITerminal.(print_string [red] "Malformed TREYREYR\n"));
    (play game)
  | Bet_Too_Low -> (ANSITerminal.(print_string [red] "Bet too low"));
    (play game)

(** [add_new_player g] adds a new player from player input to g*)
and add_new_player game =

  ANSITerminal.(print_string [default] "\nDo you want to add a bot? (y/_)\n |> ");
  match (read_line ()) with
  | "y" -> 
    let bot_name = "Bot:" ^ string_of_int (Random.int 10) in 
    (play (Blackjack.add_player (Player.new_player bot_name 
      (Chip.create_chips 10 10 10 10 10) 
      [Cards.empty] [Chip.empty] true) game))
  | _ ->
    (ANSITerminal.(print_string [default] "\nEnter your name: ");
    let name = read_line () in
    ANSITerminal.(print_string [default] "\nEnter your starting chips: ");
    let starting_chips = get_chips ()
    in 
    if (Chip.get_value starting_chips >= Blackjack.min_bet game) then
      (play (Blackjack.add_player (Player.new_player name starting_chips
                                      [Cards.empty] [Chip.empty] false) game))
    else 
      (ANSITerminal.(print_string [red] "You need to start with more money.");
        play game))

(* [main ()] prompts for the game to play, then starts it. *)
let main () =

  ANSITerminal.(print_string [magenta] "\nWelcome to the Blackjack!\n");
  print_string "Ready to play? (y/n) |> ";

  let rec start () =
    try (match (read_line ()) with
        | "y" -> 
          (ANSITerminal.(print_string [default] ("\nWhat minimum bet table do "^
                                                 "you want to sit at?\n |> "));
           let min_bet = get_num (read_line ()) in
           ANSITerminal.(print_string [default] "\nEnter your name!\n |> "); 
           let name = (read_line ()) in
           ANSITerminal.(print_string [default] "\nEnter starting chips: ");
           let init_chips = get_chips () in
           play (Blackjack.create_game [(Player.new_player name init_chips
                                           [Cards.empty] [Chip.empty] false)]
                   min_bet 6 0))
        | "n" -> (ANSITerminal.(print_string [green] "Goodbye!\n")); exit 0
        | _ -> (ANSITerminal.(print_string [red] "Malformed input")); exit 0 )
    with
    | Failure m -> start () in
  start ()

(* Execute the game engine. *)
let () = main ()
