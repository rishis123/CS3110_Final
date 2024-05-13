(* val check_master : string -> bool *)
(* [check_master pwd] returns whether or not the given [pwd] is the correct
   master password.contents *)

val string_to_sha3_hash : string -> string
(** [string_to_sha3_hash str] is the 256-bit SHA3 hash of [str]. *)

val string_to_salted_hash : string -> Bcrypt.hash
(** [string_to_salted_hash str] is the salted hash of [str]. *)

val check_master_pwd : string -> bool
(** [check_master_pwd password] is [true] if [password] hashes to the stored
    master password. *)
