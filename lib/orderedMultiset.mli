type 'a t
(** The type of a dynamically-resizable ordered multiset. *)

val make : int -> 'a t
(** [make cap] creates an array of initial capacity [cap]. *)

val insert : 'a -> 'a t -> unit
(** [insert elem set] inserts [elem] into the set *)

val find_opt : ('a -> bool) -> 'a t -> 'a option
(** [find_opt pred set] returns [Some x] if [pred x = true] for some [x] and
    [None] otherwise. *)

val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** [fold_left f init set] folds [f] over [set] with the initial value of the
    accumulator as [init] *)
