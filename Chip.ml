type color =
  | White of int
  | Red of int
  | Blue of int
  | Green of int
  | Black of int

type t = (color * color * color * color * color)

exception RI_Broken

(**RI: type t must in the form White, Red, Blue, Green, Black *)
let repo_ok = function
  |((White w),(Red r),(Blue blu),(Green g),(Black bla)) as t -> t
  | _ -> raise RI_Broken;;


let get_value = function
  |((White w),(Red r),(Blue blu),(Green g),(Black bla)) -> 
    w + r*5 + blu*10 + g*25 + bla*100
  |_ -> raise RI_Broken;;

(**[single_step t] takes the lowest denominations available and converts them
into the next denomination if possible.
Returns: type (t, bool), where the bool indicates if type *)
let single_step_simplify = function
  |((White w),(Red r),(Blue blu),(Green g),(Black bla)) as t -> 
    if w >= 5 
    then (((White (w-5)),(Red (r+1)),(Blue blu),(Green g),(Black bla)),true)
    else if r > 2
    then (((White (w)),(Red (r-1)),(Blue (blu+1)),(Green g),(Black bla)),true)
    else if r = 1 && blu >= 2
    then (((White (w)),(Red (r-1)),(Blue (blu-2)),(Green (g+1)),(Black bla)),true)
    else if g >= 4
    then (((White (w)),(Red r),(Blue blu),(Green (g-2)),(Black (bla+1))),true)
    else (t,false)
  |_ -> raise RI_Broken

  (* let single_step_break = function
    |((White w),(Red r),(Blue blu),(Green g),(Black bla)) as t -> 
    | _ -> raise RI_Broken *)

