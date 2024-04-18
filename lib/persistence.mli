val read_master_password_hash : unit -> Types.masterPasswordHash
val write_unencryptable : Types.unencryptable -> unit
val read_all_encryptable : unit -> Types.encryptable list
val write_encryptable : Types.encryptable -> unit
val delete_encryptable_by_name : string -> unit
