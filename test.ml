open OUnit2
open Chip
open Blackjack
open Cards
open Game
open Player

(**An example of how to create a blackjack game. *)
module StandardBlackjack = Blackjack.CreateGame(Classic)

let newgame = StandardBlackjack.new_game "player" Chip.empty 0 1
let test_chip = Chip.create_chips 1 2 5 6 2
let test_chip_2 = Chip.create_chips 5 6 10 2 4
let added_chips = Chip.create_chips 6 8 15 8 6
let empty_chip = Chip.empty

let standard_deck = get_standard_deck


let blackjack_tests = "Blackjack tests" >::: [
    "newgame" >:: (fun _-> assert_equal 
                      "[Name: Blackjack, Round: 0, Current player: player, First player: player]"
                      (StandardBlackjack.get_info newgame) ~printer:((fun s-> s)))
  ]

let chip_tests = "Chip tests" >:::[
    "add" >:: (fun _-> assert_equal test_chip 
                  (add test_chip empty_chip));
    "add test 2" >:: (fun _-> assert_equal added_chips 
                         (add test_chip test_chip_2));
    "bet" >:: (fun _->  assert_equal test_chip 
                  (Chip.bet added_chips test_chip_2));
    "bet" >:: (fun _-> assert_raises Not_Within 
                  ((fun () -> Chip.bet test_chip_2 added_chips)));
    "get_value" >:: (fun _-> assert_equal (1 + 10 + 50 + 150 + 200) 
                        (get_value test_chip));
    (** simplify, break *)
  ]

let deck_tests = "Deck tests" >::: [
    "standard deck, compare, shuffle " >:: (fun _-> 
        assert_equal standard_deck (standard_deck 
                                    |> shuffle 
                                    |> List.sort Cards.compare));
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


