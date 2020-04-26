type action = Hit of int | Split of int | DD of int | Insurance of int | Stand
type command = Command of action

exception Empty

exception Malformed

let parse s = 
  let string_list = String.split_on_char ' ' (String.lowercase_ascii s) in
  match string_list with
  | [] -> raise Empty
  | h::v::[] when h = "hit" -> Command (Hit (int_of_string v))
  | h::v::[] when h = "double" -> Command (DD (int_of_string v))
  | h::[] when h = "stand" -> Command (Stand)
  | h::[] -> raise Malformed
  | h::v::[] when h = "split" -> Command (Split (int_of_string v))
  | h::v::[] when h = "insurance" -> Command (Insurance (int_of_string v))
  | h::t -> raise Malformed