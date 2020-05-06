
open Blackjack
open Player
open Command
open Chip
(** get_info will be used to output the game state for debugging purposes) *)
let rec step r game =
  match (parse r) with
  | Quit -> print_string "Thanks for playing!"; exit 0
  | _ ->
  let game_after_cmd = (Blackjack.go game (parse r)) in
    (print_string (Blackjack.get_info game_after_cmd));
    print_string "> "; step (read_line ()) game_after_cmd

(* [create_players players] allows the user to add a new player to 
  list of players *)
let rec create_players (players: Player.t list) : Player.t list =
  (* [create_player] creates a player from user input*)
  let rec create_player : Player.t =
  (* [get_num pname] gets user input of number of chips of type pname*)
    let rec get_num pname =
      print_string ("Enter starting " ^ pname ^ ": ");
      let m = try int_of_string (read_line ()) with
      | Failure a -> print_string "Malformed\n"; (get_num pname)
      in m in
    print_string "Enter name: ";
    let n = read_line () in
    let w = get_num "whites" in
    let r = get_num "reds" in
    let b = get_num "blues" in
    let g = get_num "greens" in
    let bla = get_num "blacks" in
    (Player.new_player n (Chip.create_chips w r b g bla) [] [] false) in
  print_string "New player? (y/n): ";
  match read_line () with
  | "y" -> create_players ((create_player)::players)
  | _ -> players

(* [start_game s] is the main recursive loop for the game that takes a user input
    s and returns an output and prompt. *)
let start_game = 
  let players = create_players [] in
  let new_game = Blackjack.create_game players 0 6 1 in
  print_endline (Blackjack.get_info new_game);
  step (read_line ()) new_game 

(* [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "Welcome to the casino!";
  start_game

(* Execute the game engine. *)
let () = main ()
