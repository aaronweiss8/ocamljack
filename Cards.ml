
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
  make_standard_dec values []

let rec shuffle deck = 
  (**[compare_card t1 t2] retuns 1 if t1 is greater, 0 if they are equal, and -1
  if t2 is greater  *)
  let compare_card t1 t2 = 
    match (t1,t2) with
    |((c1,r1),(c2,r2)) -> if r1 > r2 then 1 else if r1 = r2 then 0 else -1 in

  deck |> List.map (fun x -> (x,Random.int 52)) |> List.sort compare_card |>
  List.map (fun (x,y) -> x)

let deal_one = function
  | [] -> None 
  | h::t -> Some (h,t)

let combine_decks dl =
  List.fold_left (fun x a -> x@a) dl []