val read_master_password_hash : unit -> Types.master_password_hash
val write_unencryptable : Types.unencryptable -> unit
val read_all_encryptable : unit -> Types.encryptable list
val write_encryptable : Types.encryptable -> unit
val delete_encryptable_by_name : string -> unit

val set_file_perms : unit -> unit
(** [set_file_perms ()] sets the data files to read and write allowed only for
    the owner. *)
