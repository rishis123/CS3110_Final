module type CommonPasswordsPath = sig
  val common_passwords_path : string
  (** [common_passwords_path] is the path to the file containing a
      newline-separated list of common passwords. *)
end

module Make : functor (_ : CommonPasswordsPath) -> sig
  val init_async : unit -> unit
  (** [init_async ()] asynchronously begins loading this module. *)

  val is_weak : string -> bool Lwt.t
  (** [is_weak pwd] returns true if [pwd] is weak. If [init_async ()] has not
      yet completed (or has not been called at all), this first initializes the
      module. *)

  val is_initialized : unit -> bool
  (** [is_initialized ()] returns true if the module is initialized. If so,
      is_weak will return very quickly. *)
end
