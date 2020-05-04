open OUnit2
open Chip
open Blackjack
open Cards
open Player

(**An example of how to create a blackjack game. *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* let newgame = StandardBlackjack.new_game "player" Chip.empty 0 1 *)

(** Example chips *)
let test_chip = Chip.create_chips 1 2 5 6 2
let test_chip_2 = Chip.create_chips 5 6 10 2 4
let added_chips = Chip.create_chips 6 8 15 8 6
let empty_chip = Chip.empty
(* Chips made specifically for simplifying and breaking*)
let swhite_chip = Chip.create_chips 5 0 0 0 0
let sred_chip = Chip.create_chips 0 2 0 0 0
let sblue_chip = Chip.create_chips 0 1 2 0 0
let sgreen_chip = Chip.create_chips 0 0 0 4 0

let bred_chip = Chip.create_chips 0 1 0 0 0
let bblue_chip = Chip.create_chips 0 0 1 0 0
let bgreen_chip = Chip.create_chips 0 0 0 1 0
let bblack_chip = Chip.create_chips 0 0 0 0 1

let standard_deck = get_standard_deck

(** Example cards and decks *)
let empty_deck = Cards.empty
let black_ace_hearts = Cards.make_card Heart Black Ace
let red_11_hearts = Cards.make_card Heart Red (Num 11) 
let red_10_hearts = Cards.make_card Heart Red (Num 10)
let added_to_deck = empty_deck |> Cards.add_to_deck black_ace_hearts |> Cards.add_to_deck black_ace_hearts 
let example_deck = Cards.empty
                   |> add_to_deck (Cards.make_card Diamond Red (Num 7))
                   |> add_to_deck (Cards.make_card Spade Black (Jack))
let combined_deck = added_to_deck 
                    |> add_to_deck (Cards.make_card Diamond Red (Num 7))
                    |> add_to_deck (Cards.make_card Spade Black (Jack))

(* Example Players *)
let player1 = Player.new_player "Aaron" added_chips [added_to_deck] [sblue_chip] false
let player1_split = Player.new_player "Aaron" added_chips
    [(empty_deck |> Cards.add_to_deck black_ace_hearts );
     (empty_deck |> Cards.add_to_deck black_ace_hearts )] [sblue_chip] false


let p2_hand = empty_deck |> Cards.add_to_deck red_10_hearts |> Cards.add_to_deck black_ace_hearts 

(** Example betlists *)
let b_lst = [Chip.empty;test_chip]
let b_lst2 = [test_chip;test_chip_2]
let b_lst3 = [bred_chip;bred_chip]

(* Example blackjack players *)
let player2 = Player.new_player "Covid" test_chip [p2_hand] [sblue_chip] false
let player3 = Player.new_player "Bryan" test_chip_2 [added_to_deck] [sblue_chip] false
let playerlist = [player1;player2;player3]
let playerlist2 = [player1_split;player2;player3]
let playerlist3 = [player2;player3]
let playerlist_with_bets = [(Player.new_player "asdf" test_chip [] [bred_chip] false);
                            (Player.new_player "asdf" test_chip [] [bred_chip] false)]
let player_bust = Player.new_player "Bryan"
    test_chip_2 [added_to_deck |> add_to_deck red_10_hearts
                 |> add_to_deck (make_card Heart Red Jack)] [sblue_chip] false
(* Blackjack Example Game *)
let newplayer = Player.new_player "asdf" test_chip [] [] false
let newplayer2 = Player.new_player "asdf" test_chip [] [Chip.empty] false
let newgame = create_game [newplayer;newplayer] 15 6 1
let game1 = Blackjack.create_game playerlist 15 6 1
let game2 = Blackjack.create_game playerlist3 15 6 1
let game_split = Blackjack.create_game playerlist2 15 6 1
let newgame2_lowbet = create_game [newplayer;newplayer] 0 6 1
let newgame3_lowbet = create_game [newplayer2;newplayer2] 0 6 1

(*let game_with_dealer_blackjack = {round=1;min_bet=10;players=playerlist;
                                  leftMostPlayer=List.hd playerlist;deck=standard_deck;dealer=
                                                                                         {name="dealer";chips=Chip.empty;hand=[(Heart, Red, Ace), (Heart, Red, King)];bet=[];bot=false}}
  let game_with_dealer_bad = {round=1;min_bet=10;players=playerlist;
                            leftMostPlayer=List.hd playerlist;deck=standard_deck;dealer=
                                                                                   {name="dealer";chips=Chip.empty;hand=[(Heart, Red, Num 2), (Heart, Red, Num 2)];bet=[];bot=false}}*)

let dealer_bad = Player.new_player "dealer" Chip.empty [[Cards.make_card Heart Red (Num 2);Cards.make_card Heart Red (Num 2)]] [Chip.empty] true
let dealer_bj = Player.new_player "dealer" Chip.empty [[Cards.make_card Heart Red Ace;Cards.make_card Heart Red King]] [Chip.empty] true
(* let game_with_dealer_bad = Blackjack.add_dealer_to_game game1 dealer_bad
let game_with_dealer_bj = Blackjack.add_dealer_to_game game2 dealer_bj *)
let chip_tests = "Chip tests" >:::[
    "repo_ok and create_chip test" >:: 
    (fun _ -> assert_equal test_chip (Chip.repo_ok test_chip));

    "get_value and empty test, should be 0" >::
    (fun _ -> assert_equal 0 (Chip.get_value empty_chip));

    "get_value of valued chip" >:: (fun _-> assert_equal (1 + 10 + 50 + 150 + 200) 
                                       (get_value test_chip));

    "makes sure simplify preserves value" >:: (fun _-> assert_equal 
                                                  (1 + 10 + 50 + 150 + 200) (get_value  (Chip.simplify test_chip 1)));

    "makes sure simplify preserves value (white)" >:: (fun _-> assert_equal 
                                                          5 (get_value  (Chip.simplify swhite_chip 1)));

    "makes sure simplify preserves value (red)" >:: (fun _-> assert_equal 
                                                        10 (get_value  (Chip.simplify sred_chip 1)));

    "makes sure simplify preserves value (blue)" >:: (fun _-> assert_equal 
                                                         25 (get_value  (Chip.simplify sblue_chip 1)));

    "makes sure simplify preserves value (green)" >:: (fun _-> assert_equal 
                                                          100 (get_value  (Chip.simplify sgreen_chip 1)));

    "makes sure break preserves value (red)" >:: (fun _-> assert_equal 
                                                     5 (get_value (Chip.break bred_chip 1)));

    "makes sure break preserves value (blue)" >:: (fun _-> assert_equal 
                                                      10 (get_value  (Chip.break bblue_chip 1)));

    "makes sure break preserves value (green)" >:: (fun _-> assert_equal 
                                                       25 (get_value  (Chip.break bgreen_chip 1)));

    "makes sure break preserves value (black)" >:: (fun _-> assert_equal 
                                                       100 (get_value  (Chip.break bblack_chip 1)));

    "add, empty test" >:: (fun _-> assert_equal test_chip 
                              (Chip.add test_chip empty_chip));
    "add test 2" >:: (fun _-> assert_equal added_chips 
                         (Chip.add test_chip test_chip_2));
    "bet" >:: (fun _->  assert_equal test_chip 
                  (Chip.bet added_chips test_chip_2));
    "bet" >:: (fun _-> assert_raises Not_Within 
                  ((fun () -> Chip.bet test_chip_2 added_chips)));
  ]
let deck_tests = "Deck tests" >::: [

    "standard deck, compare, shuffle " >:: (fun _-> 
        assert_equal true (List.fold_right (fun x y -> x && y ) 
                             (standard_deck |> shuffle |> List.map (fun x -> List.mem x standard_deck)) true));
    "add to deck, remove inst" >:: (fun _-> assert_equal 
                                       (empty |> add_to_deck black_ace_hearts)
                                       (remove_single_instance black_ace_hearts added_to_deck));
    "deal one" >:: (fun _-> assert_equal (added_to_deck, red_11_hearts)
                       (deal_one (added_to_deck |> add_to_deck red_11_hearts)));
    "transfer card" >:: (fun _-> assert_equal 
                            ((empty |> add_to_deck black_ace_hearts),(empty |> add_to_deck black_ace_hearts))
                            (transfer_card added_to_deck empty black_ace_hearts));
    "get rank" >:: (fun _-> assert_equal (Num 11) (get_rank red_11_hearts));
    "get rank" >:: (fun _-> assert_equal (Jack) (get_rank (make_card Heart Red Jack)));
    "combine deck" >:: (fun _-> assert_equal combined_deck 
                           (combine_decks [example_deck;added_to_deck])); 
  ]
let player_tests = "Player tests" >::: [
    "player name test" >:: (fun _ -> assert_equal "Aaron" (Player.name player1));

    "player get hand test" >:: (fun _ -> assert_equal [added_to_deck] (Player.get_hand player1));

    "player get chips test" >:: (fun _ -> assert_equal added_chips (Player.chips player1));

    "player chips value test" >:: (fun _ -> assert_equal 996 (Player.chips_value player1));

    "player bet test" >:: (fun _ -> assert_equal [sblue_chip] (Player.bet player1));

    "player bet value test" >:: (fun _ -> assert_equal 25 (Player.bet_value player1 0));

    "player add chips test" >:: (fun _ -> 
        assert_equal 1021 (Player.chips_value (Player.add_chips sblue_chip player1)) );

    "player bet chips test" >:: (fun _ -> 
        assert_equal 436 (Player.bet_value (Player.bet_chips test_chip 0 player1) 0 ));

    "player collect bets test" >:: (fun _ -> 
        assert_equal 1021 (Player.chips_value (Player.collect_bets player1)));

    "player lose bets test" >:: (fun _ -> 
        assert_equal 0 (Player.bet_value (Player.lose_bet 0 player1) 0 ));

    "player update hand test" >:: (fun _ ->
        assert_equal [] (Player.get_hand (Player.update_hand [] player1)));

    "player add to hand test" >:: (fun _ -> 
        assert_equal [black_ace_hearts::added_to_deck] (Player.get_hand (Player.add_to_hand black_ace_hearts 0 player1)));

    "player add bet test" >:: (fun _ -> assert_equal
                                  2 (List.length (Player.bet (Player.add_bet player1))));

  ]

let blackjack_tests = "Blackjack tests" >::: [
    "test current player in game" >:: (fun _ -> 
        assert_equal player1 (Blackjack.current_player game1));

    "test is blackjack method (player 2 should have it)" >:: (fun _ ->
        assert_equal true (Blackjack.is_blackjack player2));

    "test is_blackjack false" >::(fun _-> 
        assert_equal false (Blackjack.is_blackjack player1)); 

    "test go to next player" >:: (fun _ -> 
        assert_equal player2 (Blackjack.current_player (Blackjack.go_next_player game1)));

    "test hit" >:: (fun _-> 
        assert_equal (hit game1 0 |> current_player |> Player.get_hand |> List.hd) 
          ((Blackjack.get_deck game1 |> List.hd)::
           ((game1 |> current_player |> Player.get_hand |> List.hd))));
  (*
    "test check_hands" >:: (fun _->
        assert_equal ());*)

    "test did_bust false" >:: (fun _->
        assert_equal false (did_bust (player2 |> Player.get_hand |> List.hd)));

    " test hand_value" >:: (fun _->
        assert_equal 22 (player_bust |> Player.get_hand |> List.hd |> hand_value)
          ~printer:string_of_int);

    "test did_bust true" >:: (fun _->
        assert_equal true (did_bust (player_bust |> Player.get_hand |> List.hd)));

    (* Add test for splitting the bet into two elements of a list*)
    "test split error" >:: (fun _-> assert_raises (Cannot_Split)
                               (fun () -> split game2 0));

    "test split" >:: (fun _->
        assert_equal 2
          (split game1 0 |> current_player |> get_hand |> List.length));

    "test place_intitial_bets exn" >:: (fun _->
        assert_raises Bet_Too_Low (fun () -> place_initial_bets game1 b_lst));

    "test place_intitial_bets" >:: (fun _->
        assert_raises (Failure "Bet does not exist")
          (fun () ->  (place_initial_bets newgame2_lowbet b_lst3 |> get_players)));

    "test place_intitial_bets" >:: (fun _->
        assert_equal (playerlist_with_bets |> List.map (fun x -> Player.bet x))
          (place_initial_bets newgame3_lowbet b_lst3 |> get_players |>
           List.map (fun x -> Player.bet x)) ); 

    "test card_value" >:: (fun _-> 
        assert_equal 5 (make_card Heart Red (Num 5)|> get_rank |> card_value));

    "test card_value 2" >:: (fun _-> 
        assert_equal 10 ((make_card Spade Black Jack) |> get_rank |> card_value));

    "test insurance" >:: (fun _-> 
        assert_raises Cannot_Perform_Insurance 
          (fun () -> b_lst |> insurance game1));
    (*
        "test insurance" >:: (fun _->
            assert_equal (playerlist_insurance) ((insurance game1 b_lst3) |> get_players)) ;*)


    (* confirm player should have bet 25 before double down *)
    "test double_down" >:: (fun _-> assert_equal 50
                               (double_down game1 0 |> current_player |> Player.bet |> List.hd |> Chip.get_value));


    "test dealer" >:: (fun _-> assert_equal dealer_bad
                          (game_with_dealer_bad |> Blackjack.dealer));

    "test dealer" >:: (fun _-> assert_equal dealer_bj
                          (game_with_dealer_bj |> Blackjack.dealer));

(*
    "test get_results " >:: (fun _ -> assert_equal [[player2];[player3];[];[player1]]
                                (Blackjack.get_results game_with_dealer_bad));

    "test get_results2" >:: (fun _ -> assert_equal [[];[];[player2];[player3]]
                                (Blackjack.get_results game_with_dealer_bj)); *)
    (* check_hands *)

  ]

let test_suite = [
  (* blackjack_tests; *)
  chip_tests;
  deck_tests; 
  player_tests;
  blackjack_tests;
]

let suite = "search test suite" >::: test_suite

let _ = run_test_tt_main suite


