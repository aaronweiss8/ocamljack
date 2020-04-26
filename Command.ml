module Blackjack = struct
type action = Hit | Split of 'a | DD | Insurance 
type menu = Settings | Help
type 'a command = Command of 'a | Malformed | Empty

let parse s = 
let string_list = String.split_on_char ' ' (String.lowercase s) in
match string_list with
  | [] -> Empty
  | h::[] when h = "hit" -> Command Hit
  | h::[] when h = "DD" -> Command DD
  | h::[] when h = "split" -> Command (Command (Split "default")
  | h::[] when h = "settings" -> Command (Settings)
  | h::[] when h = "settings" -> Command (Help)
  | h::v::[] when h = "split" -> Command (Split v)
  | h::v::[] when h = "insurance" -> Command (Insurance v)
  | h::t -> Malformed

end