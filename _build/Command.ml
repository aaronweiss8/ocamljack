type action = 
  | Hit of int
  | Split of int
  | DD of int
  | Insurance of int
  | Stand
  | Quit

exception Empty

exception Malformed

let parse s = 
  let string_list = String.split_on_char ' ' (String.lowercase_ascii s) in
  match string_list with
  | [] -> raise Empty
  | h::v::[] when h = "hit" -> (Hit (int_of_string v))
  | h::v::[] when h = "double" -> (DD (int_of_string v))
  | h::[] when h = "stand" -> (Stand)
  | h::[] when h = "quit" -> (Quit)
  | h::[] -> raise Malformed
  | h::v::[] when h = "split" -> (Split (int_of_string v))
  | h::v::[] when h = "insurance" -> (Insurance (int_of_string v))
  | h::t -> raise Malformed