
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

(**[compare_card_shuffle t1 t2] retuns 1 if t1 is greater, 0 if they are equal, and -1
   if t2 is greater  *)
let compare_card_shuffle t1 t2 = 
  match (t1,t2) with
  |((c1,r1),(c2,r2)) -> if r1 > r2 then 1 else if r1 = r2 then 0 else -1

(** [compare c1 c2] returns 1 if the numeric value of c1 is greater than c2,
    0 if they are equal, and -1 if c2 is greater *)
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

let rec shuffle deck = 
  deck |> List.map (fun x -> (x,Random.int 52)) |> List.sort compare_card_shuffle |>
  List.map (fun (x,y) -> x)

let deal_one = function
  | [] -> None 
  | h::t -> Some (t,h)

let transfer_card ((d1,d2): deck * deck) (card: card) =
  if List.mem card d1 then
    let (new_d1: deck) = List.filter (fun a -> a <> card) d1 in
    let (new_d2: deck) = card::d2 in 
    (new_d1,new_d2)
  else raise Card_not_in_Deck

let combine_decks (dl: deck list) =
  let deck = List.fold_left (fun x a -> x@a) dl [] in
  match deck with
  | h::[] -> h
  | _ -> failwith "Error"

let order_hand (d: deck) = 
  List.sort compare d

let get_rank c = 
  match c.rep with
  | (_,_,Num x) -> Num x
  | (_,_,y) -> y