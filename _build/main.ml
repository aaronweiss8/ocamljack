
module StandardBlackjack = Blackjack.CreateGame(Blackjack.Classic)

open Command
(** get_info will be used to output the game state for debugging purposes) *)
let rec step r game = 
  match parse r with
  | cmd ->
    print_string "> "; step (read_line ()) (StandardBlackjack.go game cmd)

(** [play_game s] is the main recursive loop for the game that takes a user input
    s and returns an output and prompt. *)
let start_game name = 
  print_endline ("Welcome " ^ name ^ " to Blackjack.");
  print_string  "> ";
  let new_game = StandardBlackjack.new_game name Chip.empty 0 1 in
  print_endline (StandardBlackjack.get_info new_game);
  step (read_line ()) new_game 

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "Welcome to the casino!";
  print_endline "Enter the name of your player";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | room_name -> start_game room_name

(* Execute the game engine. *)
let () = main ()
