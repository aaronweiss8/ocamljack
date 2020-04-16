open OUnit2
open Chip
open Blackjack
open Cards
open Game
open Player

let blackjack_tests = [

]

let chip_tests = [

]

let deck_tests = [

]

let game_tests = [

]

let test_suite = [
  blackjack_tests;
  chip_tests;
  deck_tests;
  game_tests;
]

let suite = "search test suite" >::: test_suite

let _ = run_test_tt_main suite


