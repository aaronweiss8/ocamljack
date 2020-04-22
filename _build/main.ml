let rec step r = 
  match r with
  | _-> print_endline "Dummy step"; step (read_line ())

(** [play_game f] starts the adventure in file [f]. *)
let play_game = function
  | "Blackjack" -> print_endline "Selected blackjack!";step (read_line ())
  |  _ ->  print_endline "Unknown command";step (read_line())

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "Welcome to the casino!";
  print_endline "Enter the name of your blackjack room.";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | game -> play_game game

(* Execute the game engine. *)
let () = main ()
