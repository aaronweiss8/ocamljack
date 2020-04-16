(** Describes the chips in the casino and operations that you can do on them *)

(*abstract type representing a chip*)
type t

(** options for color type*)
type color =
    | White
    | Red
    | Blue
    | Green
    | Black

(** an int representing the quanity of a chip or chips, not value*)
type quantity = int