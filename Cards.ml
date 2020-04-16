
type color = | Red | Black

type suit = | Heart | Spade | Diamond | Club

type rank = 
  | Num of int
  | Jack
  | Queen
  | King
  | Ace

type card = {rep: (suit * color * rank)}

type deck = card list

let get_standard_deck = 
  let (res: deck) = [] in
  let values = 
    [(Num 2);(Num 3);(Num 4);(Num 5);(Num 6);(Num 7);(Num 8);(Num 9);
     (Num 10);Jack;Queen;King;Ace] in
  let rec make_standard_dec lst accum =
    match lst with
    | h::t -> 
      let to_add = [(Heart,Red,h);(Spade,Black,h);
                    (Diamond,Red,h);(Club,Black,h)] in 
      make_standard_dec t (to_add::accum)
    | _ -> accum in
  make_standard_dec values []

let deal_one = function
  | [] -> None 
  | h::t -> Some (h,t)

let rec shuffle deck = 
  deck |> List.map (fun x -> (x,Random.int 0)) |> List.sort Stdlib.compare |>
  List.map (fun (x,y) -> x)

let combine_decks dl =
  List.fold_left (fun x a -> x@a) dl []