
open Blackjack
open Player
open Command
open Chip
(* * get_info will be used to output the game state for debugging purposes)
let rec step r hand_idx game =
  match (parse r hand_idx) with
  | Quit -> print_string "Thanks for playing!\n"; exit 0
  | Stand idx -> let hand_number = (if (idx+1 = (current_player game |> get_hand |>
                                                 List.length))
                                    then 0 else idx + 1) in begin
      let game_after_cmd = (Blackjack.go game (parse r idx) false) in
      (print_endline (Blackjack.get_info game_after_cmd));
      print_endline ("Current Hand: " ^ string_of_int hand_number);
      print_string "> "; step (read_line ()) hand_number game_after_cmd end
  | exception Malformed -> print_string 
                             "Action not recognized. Please try again. \n> "; 
    step (read_line ()) hand_idx game
  | _ ->
    let game_after_cmd = (Blackjack.go game (parse r hand_idx) false |> check_hands) in
    (print_endline (Blackjack.get_info game_after_cmd));
    print_endline ("Current Hand: " ^ string_of_int hand_idx);
    print_string "> "; step (read_line ()) hand_idx game_after_cmd


(* [create_players players should_return] allows the user to add a new player
   to list of players *)
let rec create_initial_players (players: Player.t list) (should_return:bool)
  : Player.t list =
  if should_return then players else
  (ANSITerminal.(print_string [default] "Enter name: ");
  let name = (read_line ()) in
  ANSITerminal.(print_string [default] "Enter Starting Chips: ");
  let initial_chips = get_user_chip in
  let np = Player.new_player name initial_chips [] [] false in
  let np_lst = np::players in
  ANSITerminal.(print_string [green] ("\nAre there any more people who want" ^
  " to play? If so type 'y', otherwise hit any key. \n |>"));
  match (read_line ()) with
  | "y" -> create_initial_players np_lst false
  | _ -> create_initial_players np_lst true)


(* [create_players players] allows the user to add a new player to 
   list of players *)
(* let rec create_players (players: Player.t list) (no:bool) : Player.t list =
  
  let create_player_prompt = if no then "Add another player? (y/n): \n> "
  else "New player? (y/n): \n> " in

  (* [create_player] creates a player from user input*)
  let create_player : Player.t =
    (* [get_num pname] gets user input of number of chips of type pname*)
    ANSITerminal.(print_string [default] "Enter name: ");
    let n = read_line () in
    (Player.new_player n (get_user_chip) [] [] false) in

  
  ANSITerminal.(print_string [cyan] create_player_prompt);
  match (read_line ()),no with
  | "y",_ -> create_players (create_player::players) true
  | _,false -> ANSITerminal.(print_string [red] "You must add at least one player! \n");
    create_players [] false
  | "n",_ -> players
  | _,true -> ANSITerminal.(print_string [red] "Malformed \n"); create_players 
      (create_player::players) true *)

(* [start_game s] is the main recursive loop for the game that takes a user input
    s and returns an output and prompt. *)
let start_game =
  let players = (create_initial_players [] false |> List.rev) in
  (** have deal_initial_cards in as a placeholder;initial bets not implemented *)
  let new_game = Blackjack.create_game players 15 6 0 |> deal_initial_cards in
    (* initial bet -> requires bot functionality to work *)
    print_endline (new_game |> get_info);
    print_string "> ";
  step (read_line ()) 0 new_game *)

(*[simp_or_break players accum] prompts each user to simplify or break their
chips *)
let rec simp_or_break players accum =
  let sob_aux player is_simp steps = 
    if is_simp then (Player.simplify_chips steps player) 
    else (Player.break_chips steps player) in

  match players with
  | h::t -> 
    (ANSITerminal.(print_string [yellow] ("\n" ^ (Player.name h) ^ ", do you" ^ 
    " want to simplify or break your" ^ " chips and how many times each?" ^
    "\nType Break or Simplify or Pass.\n |> "));
    match (read_line ()) with
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

(*[get_chips ()] prompts a user to make a set of chips for whatever the
specific case is. *)
let get_chips () = 
  (ANSITerminal.(print_string [default] ("\nEnter the number of white chips: "));
  let w = (read_int ()) in
  ANSITerminal.(print_string [default] ("Enter the number of red chips: "));
  let r = (read_int ()) in
  ANSITerminal.(print_string [default] ("Enter the number of blue chips: "));
  let b = (read_int ()) in
  ANSITerminal.(print_string [default] ("Enter the number of green chips: "));
  let g = (read_int ()) in
  ANSITerminal.(print_string [default] ("Enter the number of black chips: "));
  let bla = (read_int ()) in
  Chip.create_chips w r b g bla)

(*[get_prer_round_bet players accum] returns a chip list of all the players
to-bet bets *)
let rec get_pre_round_bet players accum =
  match players with
  | h::t -> ANSITerminal.(print_string [blue] ("\n" ^ (Player.name h) ^ "'s bet! \n"));
    if Player.is_user h then
    let to_bet = get_chips () in
    get_pre_round_bet t (to_bet::accum)
    else get_pre_round_bet t ((create_chips 0 3 0 0 0)::accum)
  | [] -> List.rev accum

let rec dealer_turn game =
    let d_hand = game |> Blackjack.dealer |> Player.get_hand |> List.hd in
    if Blackjack.hand_value d_hand < 17 then (dealer_turn (Blackjack.hit game 0 true))
    else game

(*[play_and_rotate game ind on_dealer] does the "Heavy Lifting" for the 
functionality of the game *)
let rec play_and_rotate game ind on_dealer =
  let cp = Blackjack.current_player game in
  ANSITerminal.(print_string [magenta;Bold] ("\n" ^ (Player.name cp) ^ 
  ",what is your move?\n" ^ "The command options are hit, double down, stand,"^ 
  " quit, split, or insurance. \n|> "));
  if on_dealer then 
     let ng = dealer_turn game in
     Blackjack.get_info ng;
     ng
  else
  let input = (read_line ()) in
  match (parse input ind) with
  | Hit i -> 
      (let ng = Blackjack.hit game ind on_dealer in
      Blackjack.get_info ng;
      let new_cp = Blackjack.current_player ng in
      let hand_val = Blackjack.hand_value (List.nth (Player.get_hand new_cp) ind) in
      ANSITerminal.(print_string [cyan] ("\nYour hand is worth " ^ (string_of_int hand_val) ^ " after hitting"));
      let num_hands_of_cp = (new_cp |> Player.get_hand |> List.length) in
      if(hand_val > 21) && (ind = (num_hands_of_cp - 1)) then
        let rotated_game = (Blackjack.go_next_player ng) in
        if (Blackjack.current_player rotated_game = Blackjack.leftMostPlayer rotated_game)
          then play_and_rotate rotated_game 0 true
          else play_and_rotate rotated_game 0 false
      else if (hand_val > 21) then
        play_and_rotate ng (ind + 1) on_dealer
      else play_and_rotate ng ind on_dealer)
  | Stand i -> 
    (let hand_val = Blackjack.hand_value (List.nth (Player.get_hand cp) ind) in
    ANSITerminal.(print_string [cyan] ("\nYou chose to stand on " ^ (string_of_int hand_val)));
    Blackjack.get_info game;
    let num_hands_of_cp = (cp |> Player.get_hand |> List.length) in
    if (num_hands_of_cp - 1) = ind then
      let rotated_game = (Blackjack.go_next_player game) in
      if (Blackjack.current_player rotated_game = Blackjack.leftMostPlayer rotated_game)
                then play_and_rotate rotated_game 0 true
                else play_and_rotate rotated_game 0 false
    else play_and_rotate game (ind + 1) on_dealer)
  | DD i ->
    (let ng = Blackjack.double_down game ind in
    Blackjack.get_info ng;
    let new_cp = Blackjack.current_player ng in
    let hand_val = Blackjack.hand_value (List.nth (Player.get_hand new_cp) ind) in
    ANSITerminal.(print_string [cyan] ("\nYour hand is worth " ^ (string_of_int hand_val) ^ " after doubling down"));
    let num_hands_of_cp = (new_cp |> Player.get_hand |> List.length) in 
    if(ind = (num_hands_of_cp - 1)) then
      let rotated_game = (Blackjack.go_next_player ng) in
      if (Blackjack.current_player rotated_game = Blackjack.leftMostPlayer rotated_game)
      then play_and_rotate rotated_game 0 true
      else play_and_rotate rotated_game 0 false
    else play_and_rotate ng (ind + 1)on_dealer)
  | Split i ->
    (try (
      let ng = Blackjack.split game ind in
      ANSITerminal.(print_string [blue] "\nYou split your cards.\n");
      play_and_rotate ng ind on_dealer
    )
    with
    | Cannot_Split -> ANSITerminal.(print_string [red] "\nYou cannot split.\n");
      play_and_rotate game ind on_dealer)
  | Insurance i -> ANSITerminal.(print_string [red] "TODO");
    play_and_rotate game ind on_dealer
  | Quit -> ANSITerminal.(print_string [yellow] "Goodbye!"); exit 0
  

let rec play game =

  ANSITerminal.(print_string [green] "\nDo you want to add another player to the table? (y/n)\n>");
  try (match (read_line ()) with
  | "y" -> (ANSITerminal.(print_string [default] "\nEnter your name: ");
    let name = read_line () in
    ANSITerminal.(print_string [default] "\nEnter your starting chips: ");
    let starting_chips = get_chips ()
      in (play (Blackjack.add_player (Player.new_player name starting_chips [[]] [Chip.empty] false) game)))
  | _ ->
    let simp_break_ed_game = Blackjack.update_playerlst game (simp_or_break (Blackjack.get_players game) []) in
    (*AT THIS POINT THE USER HAS THE OPTION TO BREAK OR SIMPLIFY THEIR CHIPS *)
    Blackjack.get_info simp_break_ed_game;
    ANSITerminal.(print_string [blue] "Place initial Bets, remember the minimum bet!");
    let bets = get_pre_round_bet (Blackjack.get_players simp_break_ed_game) [] in
    let betted_game = Blackjack.place_initial_bets game bets in
    (*AT THIS POINT THE USER HAS PLACED THEIR INITIAL BET *)
    Blackjack.get_info betted_game; 
    let game_after_delt = Blackjack.deal_initial_cards betted_game in
    (*AT THIS POINT THE USER HAS BEEN DELT THEIR INITIAL CARDS *)
    Blackjack.get_info game_after_delt;
    let play_round = play_and_rotate game_after_delt 0 false in 
    (*AT THIS POINT THE ROUND HAS BEEN PLAYED *)
    Blackjack.get_info play_round;
    let round_with_check = Blackjack.check_hands play_round in
    (*AT THIS POINT THE BETS HAVE BEEN COLLECTED OR DISTRIBUTED *)
    let next_round = Blackjack.next_round round_with_check in
    Blackjack.get_info next_round;
    play next_round)
    with
      | Failure m -> (ANSITerminal.(print_string [red] "Malformed\n")); (play game)
      | Bet_Too_Low -> (ANSITerminal.(print_string [red] "Bet too low")); (play game)

 
(* [main ()] prompts for the game to play, then starts it. *)
let main () =

  ANSITerminal.(print_string [magenta] "\nWelcome to the Blackjack!\n");
  print_string "Ready to play? (y/n) |>";

  match (read_line ()) with
  | "y" -> (ANSITerminal.(print_string [default] "\nEnter your name!\n |> "); 
    let name = (read_line ()) in 
    let init_chips = get_chips () in
    play (Blackjack.create_game [(Player.new_player name init_chips [[]] [Chip.empty] false)] 15 6 0))
  | "n" -> (ANSITerminal.(print_string [green] "Goodbye!")); exit 0
  | _ -> (ANSITerminal.(print_string [red] "Malformed input")); exit 0

(* Execute the game engine. *)
let () = main ()
