type action = 
  | Hit
  | Split
  | DD
  | Stand
  | Quit

exception Empty

exception Malformed

let parse s idx = 
  let string_list = String.split_on_char ' ' (String.lowercase_ascii s) in
  match string_list with
  | [] -> raise Empty
  | h::[] when h = "hit" -> (Hit)
  | h::t::[] when (h ^ " " ^ t) = "double down" -> (DD)
  | h::[] when h = "stand" -> (Stand)
  | h::[] when h = "quit" -> (Quit)
  | h::[] when h = "split" -> (Split)
  | _ -> raise Malformed