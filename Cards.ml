type color = | Red | Black

type suit = | Heart | Spade | Diamond | Club

exception Card_not_in_Deck

type rank = 
  | Num of int
  | Jack
  | Queen
  | King
  | Ace

type card = {rep: (suit * color * rank)}

type deck = card list

(**[compare_card_shuffle t1 t2] retuns 1 if t1 is greater, 0 if they are equal,
   and -1 if t2 is greater  *)
let compare_card_shuffle t1 t2 = 
  match (t1,t2) with
  |((c1,r1),(c2,r2)) -> if r1 > r2 then 1 else if r1 = r2 then 0 else -1

let compare c1 c2 =
  match (c1,c2) with
  |({rep = (c1s,c1c,c1r)},{rep = (c2s,c2c,c2r)}) -> 
    match (c1r,c2r) with
    |((Num x),(Num y)) -> Stdlib.compare x y
    |((Num x),Jack) -> -1
    |((Num x),Queen) -> -1
    |((Num x),King) -> -1
    |((Num x),Ace) -> -1
    |(Jack,(Num y)) -> 1
    |(Queen,(Num y)) -> 1
    |(King,(Num y)) -> 1
    |(Ace,(Num y)) -> 1
    |(Jack,Jack) -> 0
    |(Queen,Queen) -> 0
    |(King,King) -> 0
    |(Ace,Ace) -> 0
    |(Jack,Queen) -> -1
    |(Jack,King) -> -1
    |(Jack,Ace) -> -1
    |(Queen, Jack) -> 1
    |(Queen, King) -> -1
    |(Queen, Ace) -> -1
    |(King, Jack) -> 1
    |(King, Queen) -> 1
    |(King, Ace) -> -1
    |(Ace, Jack) -> 1
    |(Ace, Queen) -> 1
    |(Ace, King) -> 1

let get_standard_deck = 
  let values = 
    [(Num 2);(Num 3);(Num 4);(Num 5);(Num 6);(Num 7);(Num 8);(Num 9);
     (Num 10);Jack;Queen;King;Ace] in
  let rec make_standard_dec lst accum =
    match lst with
    | h::t -> 
      let to_add = [(Heart,Red,h);(Spade,Black,h);
                    (Diamond,Red,h);(Club,Black,h)] in 
      make_standard_dec t (to_add@accum)
    | _ -> accum in
  [] |> make_standard_dec values |> List.map (fun x -> {rep=x})

let rec shuffle (deck: deck) : deck =
  Random.self_init ();
  deck |> List.map (fun x -> (x,Random.int 52)) |>
  List.sort compare_card_shuffle |>
  List.map (fun (x,y) -> x)

let add_to_deck c deck =
  c::deck

let remove_single_instance c d =
  let rec rsi_aux card deck found accum = 
    match deck with
    |h::t -> if found then rsi_aux card t true (h::accum)
      else if (compare h card) = 0 then rsi_aux card t true accum
      else rsi_aux card t false (h::accum)
    |[] -> List.rev accum in
  rsi_aux c d false []

let deal_one deck =
  match deck with
  | [] -> failwith "Need to reset deck"
  | h::t -> (t,h)

let transfer_card d1 d2 (card: card) =
  if List.mem card d1 then
    let (new_d1: deck) = remove_single_instance card d1 in
    let (new_d2: deck) = card::d2 in 
    (new_d1,new_d2)
  else raise Card_not_in_Deck

let combine_decks (dl: deck list) = List.flatten dl

let order_hand (d: deck) = 
  List.sort compare d

let get_rank c = 
  match c.rep with
  | (_,_,Num x) -> Num x
  | (_,_,y) -> y

let is_ten card =
  let c = get_rank card in
  match c with
  | Num n -> n = 10
  | Ace -> false
  | _ -> true

let get_suit_string c =
  match c.rep with
  | (Heart,_,_) -> "Hearts"
  | (Spade,_,_) -> "Spades"
  | (Diamond,_,_) -> "Diamonds"
  | (Club,_,_)  -> "Clubs"

let get_rank_string c = 
  match c.rep with
  | (_,_,Num x) -> string_of_int x
  | (_,_,Jack) -> "Jack"
  | (_,_,Queen) -> "Queen"
  | (_,_,King) -> "King"
  | (_,_,Ace) -> "Ace"

let make_card suit color rank = 
  {rep = (suit,color,rank)}

let empty : deck = []

let deck_value phand = 
  let handlist = List.map get_rank phand in
  let rec sumaces acc num =
    if num = 0 then acc else
    if num = 1 && acc + 11 <= 21 then acc + 11 else
    if 11 + (num-1) + acc <= 21 then acc + 11 + num - 1 else
      acc + num in
  let rec sumhand (hand: rank list) acc num_aces =
    match hand with
    | [] -> if num_aces = 0 then acc else (sumaces acc num_aces)
    | h::t -> match h with
      | Num x -> (sumhand t (acc + x) num_aces)
      | Ace -> (sumhand t acc (num_aces + 1))
      | _ -> (sumhand t (acc + 10) num_aces) in
  sumhand handlist 0 0

(**[double_or d c a] doubles the bet on a condition relative to the value of
   the dealer's card [dv]*)
let double_or condition action = 
  if (condition) then "double down" else action

(**[split_or d c a] splits the player's hand on a condition relative to the 
   value of the dealer's card [dv], and otherwise returns another action
   [action]*)
let split_or d condition action =
  if (not (d = 11))then
    if (condition) then "split" else action
  else 
    "hit"

(**[soft_recommendation pv d dv] gives a recommendation to a user with a
   soft hand, conditional to the value of the dealer's card [dv]*)
let soft_recommendation pv dv =
  match pv with
  | 13
  | 14 
  | 15
  | 16 -> double_or (dv > 3 && dv < 7) "hit"
  | 17 -> double_or (dv < 7) "hit"
  | 18 -> double_or (dv > 2 && dv < 7) "stand"
  | 19 -> double_or ((dv=6)) "stand"
  | _ -> "stand"


(**[split_recommendation pv d dv] recommends whether a user should split a pair,
   conditional to the value of the dealer's card [dv]*)
let split_recommendation card dv =
  match card with
  | 2 -> split_or dv (dv > 2 && dv < 8) "hit"
  | 3 -> split_or dv (dv >= 4 && dv <= 7) "hit"
  | 4
  | 5 -> double_or (dv <10) "hit"
  | 6 -> split_or dv (dv >= 3 && dv <= 6) "hit"
  | 7 -> split_or dv (dv <= 7) "hit"
  | 8 -> "split"
  | 9 -> split_or dv ((dv <= 6) || (dv = 7) || (dv = 8)) "stand"
  | 10 -> "stand"
  | _ -> failwith "Should never happen"

(** [hit d c a] hits on a condition relative to the value of
    the dealer's card *)
let hit_or condition action = 
  if (condition) then "hit" else action

let hard_recommendation pv dv =
  if pv <= 7 then "hit" else
    match pv with
    | 4
    | 5
    | 6
    | 7 -> "hit"
    | 8 -> double_or (dv < 7 && dv > 4) "hit" 
    | 9 -> double_or (dv < 8) "hit" 
    | 10 -> double_or (dv < 10) "hit"
    | 11 -> "double down"
    | 12 -> hit_or (dv < 7 && dv > 3) "stand"
    | 13 -> hit_or (dv >= 7) "stand"
    | 14 -> hit_or (dv >= 7) "stand"
    | 15 -> hit_or (dv >= 7) "stand"
    | 16 -> hit_or (dv >= 7) "stand"
    | _ -> "stand"

let check_if_soft hand =
  let rec add_aces v num =
    if num = 1 then v + 11 <= 21 else
      add_aces (v + 1) (num - 1) in
  let ranks = List.map (fun x -> get_rank x) hand in
  let hand_without_aces = List.filter (fun x -> x <> Ace) ranks in
  let num_aces = List.fold_left (fun a x -> if x = Ace then a+1 else a)
      0 ranks in
  (*print_string ("FUCKFUCKFUCKFUCK" ^ string_of_int num_aces);*)
  let haval = deck_value (List.map (fun x -> {rep=(Heart, Red, x)})
                            hand_without_aces)
  in
  if num_aces > 0 then add_aces haval num_aces else false

let recommendation player_hand dealer_hand = 
  let dealer_value = [(List.nth dealer_hand 1)] |> deck_value in
  let player_value = deck_value player_hand in

  match (player_hand, dealer_value) with
  | ((a::b::[]), d) -> 
    (match (get_rank a, get_rank b) with
     | (Ace, Ace) -> "split"
     | (Num n1, Num n2) when n1 = n2 -> split_recommendation n1 dealer_value
     | _ , _-> (if check_if_soft player_hand then
      soft_recommendation player_value dealer_value
    else hard_recommendation player_value dealer_value))
  | ((h::t), d) ->
    if check_if_soft player_hand then
      soft_recommendation player_value dealer_value
    else hard_recommendation player_value dealer_value
  | _, _-> failwith "Not possible"

let rec to_string hand acc =
match hand with
| [] -> acc
| h::[] -> to_string t (get_rank_string h ^ " of " ^ get_suit_string h ^ acc)
| h::t -> to_string t 
(get_rank_string h ^ " of " ^ get_suit_string h ^ acc ^ ", ")