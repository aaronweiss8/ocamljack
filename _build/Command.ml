type action = 
  | Hit of int
  | Split of int
  | DD of int
  | Insurance of int
  | Stand of int
  | Quit

exception Empty

exception Malformed

let parse s idx = 
  let string_list = String.split_on_char ' ' (String.lowercase_ascii s) in
  match string_list with
  | [] -> raise Empty
  | h::[] when h = "hit" -> (Hit idx)
  | h::t::[] when (h ^ " " ^ t) = "double down" -> (DD idx)
  | h::[] when h = "stand" -> (Stand idx)
  | h::[] when h = "quit" -> (Quit)
  | h::[] when h = "split" -> (Split idx)
  | h::[] when h = "insurance" -> (Insurance idx)
  | _ -> raise Malformed