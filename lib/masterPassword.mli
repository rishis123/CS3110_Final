val string_to_salted_hash : string -> Types.unencryptable
(** [string_to_salted_hash str] is the salted hash of [str]. *)

val check_master_pwd : string -> bool
(** [check_master_pwd password] is [true] if [password] hashes to the stored
    master password. *)
