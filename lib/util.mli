val non_empty_or_none : string -> string option
(** [non_empty_or_none str] returns [Some str] if [str] is not empty (<> "") and
    [None] otherwise *)

val min : 'a list -> 'a
(** [min list] returns the minimum element in [list] *)

(* val min_by : ('a -> 'b) -> 'a list -> 'a (** [min_by selector list] returns
   the element [x] in [list] for which [selector x] is at a minimum *) *)

val uncurry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** [uncurry f] returns the uncurried form of f *)

val fuzzy_equal : string -> string -> bool
(** [fuzzy_equal x y] returns whether x and y are essentially equal *)

val zip : 'a list -> 'b list -> ('a * 'b) list
(** [zip l1 l2] returns a list of tuples, with the ith tuple containing the ith
    element of l1 and the ith element of l2 *)
