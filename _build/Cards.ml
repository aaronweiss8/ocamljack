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
  
let rec_aux player_value dealer_value = 
  if player_value <= 8 then "hit"
  else if (player_value = 9) then
    if dealer_value >= 3 && dealer_value <= 6 then "double down"
    else "hit"
  else if (player_value = 10) then
    if dealer_value <= 9 then "double down"
    else "hit"
  else if (player_value = 11) then 
    if dealer_value <= 10 then "double down"
        else "hit"
    
let recommendation player_hand dealer_hand = 

  let dealer_value = (List.nth dealer_hand 1) in

  match (player_hand, dealer_value) with
  | ((a::b::[]), d) -> 
    match (get_rank a, get_rank b) with
    | (Ace, Ace) -> "stand"
    | (Num n1, Num n2) ->
      if n1 = n2 then
        match n1 with
        | 2
        | 3 -> (if d <> Ace then
                match d with
                | Num dv -> if (dv >= 4  && dv <= 7) then "split" else "hit"
                | _ -> failwith "should never happen"
              else 
              "hit")
        | 4
        | 5 -> "hit"
        | 6 -> (if d <> Ace then
                match d with
                | Num dv -> if (dv >= 3  && dv <= 6) then "split" else "hit"
                | _ -> failwith "should never happen"
              else 
              "hit")
        | 7 -> (if d <> Ace then
                match d with
                | Num dv -> if (dv <= 7) then "split" else "hit"
                | _ -> failwith "should never happen"
              else 
              "hit")
        | 8 -> "split"
        | 9 -> (if d <> Ace then
                match d with
                | Num dv -> if (dv <= 6) || (dv = 7) || (dv = 8) then "split"
                   else "stand"
                | _ -> failwith "should never happen"
              else 
              "hit")
        | 10 -> "stand"
        | _ -> failwith "Should never happen"
      else 
        let hand_val = deck_value player_hand in
    | (Ace, Num n)
    | (Num n, Ace)
      else
        let hand_val = deck_value player_hand in

  | ((h::t), d) ->
    