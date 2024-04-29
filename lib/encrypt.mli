open Types

val set_key : string -> unit
(** [set_key pwd] sets the master password to the given [pwd] *)

val encrypt : encryptable -> encrypted
(** [encrypt e] encrypts the given [e] to a string *)

val decrypt : encrypted -> encryptable
(** [decrypt s] decrypts the given [s]. Returns a password if [s] is an
    encrypted password and a login if it is an encrypted login *)

val salt_hash : string -> unencryptable
(** [salt_hash s] salts the given string. Assumes the given string is a hash *)
