open Types

val unencryptable_to_string : unencryptable -> string
(** [unencryptable_to_string u] is the contents of [Types.MasterPasswordHash]. *)

val unencryptable_of_string_opt : string -> unencryptable option
(** [unencryptable_of_string str] is the [Types.MasterPasswordHash] that is
    represented by [str]. Returns {!None} if [str] does not represent any
    [Types.MasterPasswordHash]. *)

val encryptable_to_string : encryptable -> string
(** [encryptable_to_string e] is a reversable string representation of [e]. *)

val encryptable_of_string_opt : string -> encryptable option
(** [encryptable_of_string str] is the [Types.encryptable] that is represented
    by [str]. Returns {!None} if [str] does not represent any
    [Types.encryptable]. *)

val json_of_encrypted : encrypted -> Yojson.Basic.t
(** [json_of_encrypted enc_data] is the json representing [enc_data]. *)

val encrypted_of_json : Yojson.Basic.t -> encrypted
(** [encrypted_of_json json] is the encrypted data represented by [json]. *)
