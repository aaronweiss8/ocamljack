(** The module [Command] contains the representation for a user
    command as well as the method to convert user input into a command*)

(** The module type that defines the parsing rules for Blackjack. *)
type action = Hit 
            | Split  
            | DD 
            | Stand
            | Quit

(** [parse s i] returns an action corresponding to hand i *)
val parse : string -> int -> action

exception Empty

exception Malformed
