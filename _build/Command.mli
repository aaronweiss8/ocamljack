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
