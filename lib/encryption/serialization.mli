val unencryptable_to_string : Types.unencryptable -> string
(** [unencryptable_to_string u] is the contents of [Types.MasterPasswordHash]. *)

val unencryptable_of_string_opt : string -> Types.unencryptable option
(** [unencryptable_of_string str] is the [Types.MasterPasswordHash] that is
    represented by [str]. Returns {!None} if [str] does not represent any
    [Types.MasterPasswordHash]. *)

val encryptable_to_string : Types.encryptable -> string
(** [encryptable_to_string e] is a reversable string representation of [e]. *)

val encryptable_of_string_opt : string -> Types.encryptable option
(** [encryptable_of_string str] is the [Types.encryptable] that is represented
    by [str]. Returns {!None} if [str] does not represent any
    [Types.encryptable]. *)
