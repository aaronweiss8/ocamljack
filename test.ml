open OUnit2
open Chip
open Blackjack
open Cards
open Game
open Player

(**An example of how to create a blackjack game. *)
module StandardBlackjack = Blackjack.CreateGame(Classic)

let newgame = StandardBlackjack.new_game "New Game"

let blackjack_tests = "Blackjack tests" >::: [
    (** "newgame" >:: (fun _-> assert_equal 
                      "[New Game, 0, 0]" (StandardBlackjack.get_info newgame))*)


  ]

let chip_tests = "Chip tests" >:::[

  ]

let deck_tests = "Deck tests" >::: [

  ]

let game_tests = "Game tests" >::: [

  ]

let test_suite = [
  blackjack_tests;
  chip_tests;
  deck_tests;
  game_tests;
]

let suite = "search test suite" >::: test_suite

let _ = run_test_tt_main suite


