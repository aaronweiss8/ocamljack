type color =
  | White of int
  | Red of int
  | Blue of int
  | Green of int
  | Black of int

type t = (color * color * color * color * color)

exception RI_Broken

exception Not_Within


(**RI: type t must in the form White, Red, Blue, Green, Black *)
let repo_ok = function
  |((White w),(Red r),(Blue blu),(Green g),(Black bla)) as t -> t
  | _ -> raise RI_Broken;;

let empty = ((White 0),(Red 0),(Blue 0),(Green 0),(Black 0))

let get_value = function
  |((White w),(Red r),(Blue blu),(Green g),(Black bla)) -> 
    (w + r*5 + blu*10 + g*25 + bla*100)
  |_ -> raise RI_Broken;;

(**[single_step t] takes the lowest denominations available and converts them
   into the next denomination if possible.
   Returns: type (t, bool), where the bool indicates if t changed*)
let single_step_simplify = function
  |((White w),(Red r),(Blue blu),(Green g),(Black bla)) as t -> 
    if w >= 5 
    then (((White (w-5)),(Red (r+1)),(Blue blu),(Green g),(Black bla)),true)
    else if r > 2
    then (((White (w)),(Red (r-2)),(Blue (blu+1)),(Green g),(Black bla)),true)
    else if r = 1 && blu >= 2
    then (((White (w)),(Red (r-1)),(Blue (blu-2)),(Green (g+1)),
           (Black bla)),true)
    else if g >= 4
    then (((White (w)),(Red r),(Blue blu),(Green (g-4)),(Black (bla+1))),true)
    else (t,false)
  |_ -> raise RI_Broken

(**[single_step_break t] takes the highest demoninaitons availabble and breaks
   into smaller ones if possible.
   Returns: type (t, bool), where the bool indicates if t changed*)
let single_step_break = function
  |((White w),(Red r),(Blue blu),(Green g),(Black bla)) as t -> 
    if bla >=1 
    then (((White w),(Red r),(Blue blu),(Green (g+4)),(Black (bla-1))),true)
    else if g >= 1
    then (((White w),(Red (r+1)),(Blue (blu+2)),(Green (g-1)),
           (Black bla)),true)
    else if blu >= 1
    then (((White w),(Red (r+2)),(Blue (blu-1)),(Green g),(Black bla)),true)
    else if r >= 1
    then (((White (w+5)),(Red (r-1)),(Blue blu),(Green g),(Black bla)),true)
    else (t,false)
  | _ -> raise RI_Broken

let simplify t steps =
  let rec simplify_aux chips s_tg = 
    let simp_chip = single_step_simplify chips in
    match simp_chip with
    |(t,true) -> if s_tg = 0 then t else simplify_aux t (s_tg - 1)
    |(t,false) -> t in
  simplify_aux t steps

let break t steps =
  let rec break_aux chips s_tg =
    let break_chip = single_step_break chips in
    match break_chip with
    |(t,true) -> if s_tg = 0 then t else break_aux t (s_tg - 1)
    |(t,false) -> t in
  break_aux t steps

let add t1 t2 =
  match (t1,t2) with
  |(((White w1),(Red r1),(Blue blu1),(Green g1),(Black bla1)),
    ((White w2),(Red r2),(Blue blu2),(Green g2),(Black bla2))) ->
    ((White (w1+w2)),(Red (r1+r2)),(Blue (blu1+blu2)),(Green (g1+g2)),
     (Black (bla1+bla2)))
  | _ -> raise RI_Broken

let bet t1 t2 = 
  match (t1,t2) with
  |(((White w1),(Red r1),(Blue blu1),(Green g1),(Black bla1)),
    ((White w2),(Red r2),(Blue blu2),(Green g2),(Black bla2))) ->
    let new_w = w1 - w2 in 
    let new_r = r1 - r2 in
    let new_blu = blu1 - blu2 in
    let new_g = g1 - g2 in
    let new_bla = bla1 - bla2 in
    if new_w >= 0 && new_r >= 0 && new_blu >= 0 && new_g >= 0 && new_bla >= 0
    then ((White new_w),(Red new_r),(Blue new_blu),(Green new_g),
          (Black new_bla))
    else raise Not_Within  
  | _ -> raise RI_Broken

let create_chips w r b g bl = (White w), (Red r), (Blue b), (Green g),
                              (Black bl)

let is_within c1 c2 =
  match (c1,c2) with
  |(((White w1),(Red r1),(Blue blu1),(Green g1),(Black bla1)),
    ((White w2),(Red r2),(Blue blu2),(Green g2),(Black bla2))) ->
    if (w1 >= w2) && (r1 >= r2) && (blu1 >= blu2) && (g1 >= g2) &&
       (bla1 >= bla2) then
      true else false
  | _ -> failwith "RI Broken"

let to_string c =
  match c with
  | (White w,Red r,Blue b,Green g, Black bl) ->
    "[Whites: " ^ string_of_int w ^ ", Reds: " ^ string_of_int r ^ ", Blues: " ^
    string_of_int b ^ ", Greens: " ^ string_of_int g ^ ", Blacks: " ^ 
    string_of_int bl ^ "]"
  | _ -> failwith "out of order"

let create_bot_bet min_bet chips =
  match chips with 
  | (White w,Red r,Blue b,Green g,Black bl)-> begin
      if min_bet <= w then (print_string "Bet whites!"; 
                            create_chips w 0 0 0 0)
      else
      if min_bet <= (w + 5*r) then (print_string "Bet whites and reds!";
                                    create_chips w r 0 0 0)
      else
      if min_bet <= (w + 5*r + 10*b) then (print_string "Bet w r b !";
                                           create_chips w r b 0 0)
      else
      if min_bet <= (w + 5*r + 10*b + 25*g) then (print_string "Bet w r b g!";
                                                  create_chips w r b g 0 )
      else create_chips w r b g bl
    end
  | _ -> failwith "Should not happen"

let rec create_bot_chips min_bet start =
  match start with
  | (White w),(Red r),(Blue blu),(Green g),(Black bla) ->
    if get_value start >= min_bet then start else
      create_bot_chips min_bet
        (create_chips (w*2) (r*2) (blu*2) (g*2) (bla*2))
  | _ -> failwith "Shouldnt happen"