type encrypted =
  | EncryptedString of {
      name : string;
      encrypted_data : string;
    }

val set_key : string -> unit
(** [set_key pwd] sets the master password to the given [pwd] *)

val encrypt : Types.encryptable -> encrypted
(** [encrypt e] encrypts the given [e] to a string *)

val decrypt_password : encrypted -> Types.password
(** [decrypt_password s] decrypts the given [s]. Assumes s is an encrypted
    password *)

val decrypt_login : encrypted -> Types.login
(** [decrypt_login s] decrypts the given [s]. Assumes s is an encrypted login *)

val salt_hash : string -> Types.unencryptable
(** [salt_hash s] salts the given string. Assumes the given string is a hash *)
