(** Describes the chips in the casino and operations that you can do on them *)

(*abstract type representing a chip*)
type t

(** options for color type, white is always worth 1, Red worth 5, Blue worth 10,
Green worth 25, and Black worth 100, the int value is the quanity of chip*)
type color =
    | White of int
    | Red of int
    | Blue of int
    | Green of int
    | Black of int

exception RI_Broken

(**RI: type t must in the form White, Red, Blue, Green, Black. Returns type t
if RI is okay and a failure mesage otherwise.*)
val repo_ok: t -> t

(**[get_value t] is the integer representation of the value of the chips *)
val get_value: t -> int