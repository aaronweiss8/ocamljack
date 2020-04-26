open Game
open Cards
open Player
open Command

type t
type player
type deck
type chip
exception Cannot_Split
exception Bet_Too_Low
exception Cannot_Perform_Insurance
val get_info : t -> string
val go : t -> Command.command -> t
val create_game : Player.t list -> int -> int -> int -> t
val current_player: t -> Player.t
val get_deck : t -> Cards.deck
val is_blackjack : Player.t -> bool
val go_next_player : t -> t
val hit : t -> int -> t
val card_value : Cards.rank -> int
val hand_value : Cards.deck -> int
val get_results : t -> player list list
val did_bust : Cards.deck -> bool
val split : t -> int -> t
val double_down : t -> int -> t
val next_round : t -> t
val insurance : t -> Chip.t list -> t
val place_initial_bets : t -> Chip.t list -> t
val check_hands : t -> t
val create_game : Player.t list -> int -> int -> int -> t
(* val get_command : 'a -> Command.command *)