val non_empty_or_none : string -> string option
(** [non_empty_or_none str] returns [Some str] if [str] is not empty (<> "") and
    [None] otherwise *)

val min : float list -> float
(** [min list] returns the minimum element in [list] *)

val uncurry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** [uncurry f] returns the uncurried form of f *)

val fuzzy_equal : string -> string -> bool
(** [fuzzy_equal x y] returns whether x and y are essentially equal *)
