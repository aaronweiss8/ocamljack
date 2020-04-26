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

(* [get_info t] returns a string of the current game state *)
val get_info : t -> string

(* [go t c] implements the given command on the game *)
val go : t -> Command.command -> t

(* [create_game p b d r] creates a game with players p, minimum bet b
    number of standard decks d starting at round r*)
val create_game : Player.t list -> int -> int -> int -> t

(* [current_player t] returns the current player of game t *)
val current_player: t -> Player.t

(* [get_deck t] returns the game deck for t *)
val get_deck : t -> Cards.deck

(* [is_blackjack p] returns whether player p has achieved blackjack on any
    hand - should be turned into hand_has_blackjack*)
val is_blackjack : Player.t -> bool

(* [go_next_player t] returns a game state where the next player is first*)
val go_next_player : t -> t

(* [hit t i] returns a game state where current player has hit on hand i*)
val hit : t -> int -> t

(* [card_value c] returns an int corresponding to the rank c
    current has aces only 1*)
val card_value : Cards.rank -> int

(* [hand_value h] returns the int the hand evaluates to*)
val hand_value : Cards.deck -> int

(* [get_results t] returns a list containing lists of players who:
    blackjacked against the dealer
    won against the dealer
    pushed against the dealer
    lost against the dealer
    should be changed to list of tuples of (hand, player), with results*)
val get_results : t -> player list list

(* [did_bust h] returns whether hand h busted *)
val did_bust : Cards.deck -> bool

(* [split t i] splits current players hand i, and returns game state *)
val split : t -> int -> t

(* [double_down t i] doubles down on current players hand i,
    and returns game state*)
val double_down : t -> int -> t

(* [next_round t] returns game state on the next round*)
val next_round : t -> t

(* [insurance t c] returns state given placed insurances*)
val insurance : t -> Chip.t list -> t

(* [place_initial_bets t c] returns state with placed player bets on hands*)
val place_initial_bets : t -> Chip.t list -> t

(* [check_hands t] returns game state with collected bets on hands *)
val check_hands : t -> t

(* val get_command : 'a -> Command.command *)