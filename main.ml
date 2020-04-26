
module StandardBlackjack = Blackjack.CreateGame(Blackjack.Classic)

open Command
(** get_info will be used to output the game state for debugging purposes) *)
let rec step r game = 
  match parse r with
  | cmd ->
    print_string "> "; step (read_line ()) (StandardBlackjack.go game cmd)

let rec create_players players =
    let rec create_player =
    print_string "Enter name: ";
    let n = read_line () in
    print_string "Enter whites: ";
    let w = try int_of_string read_line () with
    | Failure "int_of_string" -> (print_string "Malformed\n"); create_player
    print_string "Enter reds: ";  
    let r = try int_of_string read_line () with
    | Failure "int_of_string" -> (print_string "Malformed\n"); create_player
    print_string "Enter blues: ";
    let b = try int_of_string read_line () with
    | Failure "int_of_string" -> (print_string "Malformed\n"); create_player
    print_string "Enter greens: ";
    let g = try int_of_string read_line () with
    | Failure "int_of_string" -> (print_string "Malformed\n"); create_player
    print_string "Enter blacks: ";
    let bla = try int_of_string read_line () with
    | Failure "int_of_string" -> (print_string "Malformed\n"); create_player
    in {name=n; chips = (White w, Red r, Blue b, Green g, Black bla); hand=[];
        bet =[]; bot=false} in
  print_string "New player? (y/n): ";
  match read_line () with
  | "y" -> create_players create_player::players
  | _ -> players

(** [play_game s] is the main recursive loop for the game that takes a user input
    s and returns an output and prompt. *)
let start_game = 
  let players = create_players [] in
  let new_game = StandardBlackjack.new_game players Chip.empty 2 1 in
  print_endline (StandardBlackjack.get_info new_game);
  step (read_line ()) new_game 




(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "Welcome to the casino!";
  start_game

(* Execute the game engine. *)
let () = main ()
