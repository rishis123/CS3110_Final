val init : unit -> unit
(** [init ()] initializes this module and loads commonly used passwords into
    memory. Must be called before [is_weak]. *)

val is_weak : string -> bool
(** [is_weak pwd] returns true if [pwd] is weak. Fails if [init] has not yet
    been called. *)
