
open Blackjack
open Player
open Command
open Chip
(** get_info will be used to output the game state for debugging purposes) *)
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

let rec get_num pname =
    print_string ("Enter " ^ pname ^ ": ");
    let m = try int_of_string (read_line ()) with
      | Failure a -> print_string "Malformed\n"; (get_num pname)
    in m

let get_user_chip =
    let w = get_num "whites" in
    let r = get_num "reds" in
    let b = get_num "blues" in
    let g = get_num "greens" in
    let bla = get_num "blacks" in
    (Chip.create_chips w r b g bla)

(* [create_players players] allows the user to add a new player to 
   list of players *)
let rec create_players (players: Player.t list) (no:bool) : Player.t list =
  (* [create_player] creates a player from user input*)
  let rec create_player (no:bool): Player.t =
    (* [get_num pname] gets user input of number of chips of type pname*)
    print_string "Enter name: ";
    let n = read_line () in
    (Player.new_player n (get_user_chip) [] [] false) in
  let create_player_prompt = 
    match no with
    | false -> "New player? (y/n): \n> "
    | true  -> "Add another player? (y/n): \n> "
  in ANSITerminal.(print_string [cyan] create_player_prompt);
  match (read_line ()),no with
  | "y",_ -> create_players ((create_player true)::players) true
  | _,false -> ANSITerminal.(print_string [red] "You must add at least one player! \n");
    create_players [] false
  | "n",_ -> players
  | _,true ->  print_string "Malformed \n"; create_players 
      ((create_player true)::players) true

(* [get_player_bets p []] returns a list of chip.t that corresponds to each
  user's initial bet*)
let rec get_player_bets players acc =
  ANSITerminal.(print_string [blue] "Enter Round Bet:\n";
  match players with
  | h::t -> if Player.is_user h then get_player_bets t (get_user_chip::acc)
    else get_player_bets t ((create_chips 0 3 0 0 0)::acc)
  | [] -> List.rev acc

(* [start_game s] is the main recursive loop for the game that takes a user input
    s and returns an output and prompt. *)
let start_game = 
  ANSITerminal.(print_string [magenta] "\nWelcome to the Blackjack!\n");
  let players = (create_players [] false) |> List.rev in
  (** have deal_initial_cards in as a placeholder;initial bets not implemented *)
  let new_game = Blackjack.create_game players 15 6 0 |> deal_initial_cards in
  let bs = get_player_bets (Blackjack.get_players new_game) [] in
  let new_game = Blackjack.place_initial_bets new_game bs in
    (* initial bet -> requires bot functionality to work *)
    print_endline (new_game |> get_info);
    print_string "> "; in
  step (read_line ()) 0 new_game

(* [main ()] prompts for the game to play, then starts it. *)
let main () =
  start_game

let rec dealer_turn game =
  let d_hand = game |> Blackjack.dealer |> Player.get_hand in
  if d_hand |> List.hd |> Blackjack.hand_value < 17 then
  dealer_turn (Blackjack.go game (Hit 0) true) else
  game

(* Execute the game engine. *)
let () = main ()
