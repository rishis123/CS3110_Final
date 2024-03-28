val read_master_password_hash : unit -> Types.unencryptable
val write_unencryptable : Types.unencryptable -> unit
val read_all_encryptable : unit -> Types.encryptable list
val write_encryptable : Types.encryptable -> unit
