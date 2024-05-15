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

val zip : 'a Seq.t -> 'b Seq.t -> ('a * 'b) Seq.t
(** [zip s1 s2] returns a sequence of tuples, with the ith tuple containing the
    ith element of s1 and the ith element of s2 *)

val compare_by : ('a -> 'b) -> 'a -> 'a -> int
(** [compare_by selector x y] compares [x] and [y] by the value of [selector x]
    and [selector y] *)

val sorted_by_below_threshold : ('a -> 'b) -> 'b -> 'a Seq.t -> 'a Seq.t
(** [sorted_by_below_threshold selector threshold list] returns a sequence of
    elements of [list] for which [selector element <= threshold]. The elements
    are returned in decreasing value of [selector element] *)
