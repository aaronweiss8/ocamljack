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

exception Not_Within

(**RI: type t must in the form White, Red, Blue, Green, Black. Returns type t
   if RI is okay and a failure mesage otherwise.*)
val repo_ok: t -> t

(** empty chip representation *)
val empty : t

(**[get_value t] is the integer representation of the value of the chips *)
val get_value: t -> int

(** [simplify t steps] simplifies the chips by the single_step_simplify function
    [steps] times or until it can no longer be simplified.*)
val simplify: t -> int -> t

(** [break t steps] breaks larger denominations using he single_step_break function
    [steps] times or until it can no longer be broken. *)
val break: t -> int -> t

(**[add t1 t2] is the combined quantities in t2 to t1. 
   Raises: RI_Broken if the Representation invariant is broken*)
val add: t -> t -> t

(** [bet t1 t2] removed t2 from t1
    Returns: t1 with t2's quantities removed
    Requires: t2 be a subset of t1
    Raises: Not_Within if t2 is not contained within t1, or RI_Broken if the
    representation invariant is broken*)
val bet: t -> t -> t