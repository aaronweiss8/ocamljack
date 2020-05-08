
(** Represents the color of a card*)
type color = | Red | Black

(** Represents the suit of the card*)
type suit = | Heart  | Spade | Diamond | Club

exception Card_not_in_Deck

(** Represents the card rank*)
type rank = 
  | Num of int
  | Jack
  | Queen
  | King
  | Ace

(** Tuples of suit and rank to represent a card*)
type card

(** The deck contains a specified amount of cards to start*)
type deck = card list

(** [compare c1 c2] returns 1 if the numeric value of c1 is greater than c2,
    0 if they are equal, and -1 if c2 is greater *)
val compare : card -> card -> int

(** Returns one standard 52 card deck*)
val get_standard_deck: deck

(** Randomizes the order of the cards in the game deck*)
val shuffle: deck -> deck

(* [add_to_deck c deck] returns [deck] including [c] *)
val add_to_deck : card -> deck -> deck

(* [remove_single_instance c d] returns 
   [d] with one instance of [c] removed *)
val remove_single_instance : card -> deck -> deck

(** Removes the top card from the deck and returns it*)
val deal_one: deck -> (deck * card)

(** [transfer_card d1 d2 card] moves [card] from [d1] to [d2]
    Requires the card to be in the first deck
    Raises exception Card_not_in_deck*)
val transfer_card: deck -> deck -> card -> (deck*deck)

(** Combines decks into one for the game if game is played with several decks*)
val combine_decks: deck list -> deck

(**[order_hand d] is a sorted deck*)
val order_hand: deck -> deck

(** [get_rank c] returns the rank of card c *)
val get_rank: card -> rank

(** [empty] returns an empty deck*)
val empty : deck

(** [make_card suit color rank] returns a new card. FOR TESTING ONLY*)
val make_card : suit -> color -> rank -> card

(** [get_suit_string c] returns the suit of card [c] as a string. *)
val get_suit_string : card -> string

(** [get_rank_string c] returns the rank of card [c] as a string. *)
val get_rank_string : card -> string

(** [recommendation p d] returns a recommendation of the best move a player
should take based on the current user hand and dealer hand.] *)
(* val recommendation: hand -> hand -> string *)

