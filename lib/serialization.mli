val unencryptable_to_string : Types.unencryptable -> string
(** [unencryptable_to_string u] is the contents of [Types.MasterPasswordHash].*)

val encryptable_to_string : Types.encryptable -> string
(** [encryptable_to_string e] is the contents of [Types.Encryptable] separated
    by commas with named fields. Example: If e is a [Types.Password] and
    [e.name = "Beans"] and [e.password = "pwd123"], the
    [encryptable_to_string e = "name = Beans, password = pwd123"]*)
