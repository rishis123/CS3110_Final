val init_async : unit -> unit
(** [init_async ()] asynchronously begins loading this module. *)

val is_weak : string -> bool Lwt.t
(** [is_weak pwd] returns true if [pwd] is weak. If [init_async ()] has not yet completed (or has not been called at all), this first initializes the module. *)
