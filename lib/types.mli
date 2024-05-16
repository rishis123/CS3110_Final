type master_password_hash = Bcrypt.hash
(** [master_password_hash] is the type of a hash of the master password, used
    for checking validity at startup *)

type unencryptable =
  | MasterPasswordHash of master_password_hash
      (** [unencryptable] is the type of any storable data that is not
          encrypted. *)

type password = {
  name : string;
  password : string;
}
(** [password] is the type of a simple named password. Note that the master
    password is never represented as a [password], and is in fact never stored *)

type login = {
  name : string;
  username : string;
  password : string;
  url : string option;
}
(** [login] is the type of a login, which contains richer information than just
    a password *)

type encryptable =
  | Password of password
  | Login of login
      (** [encryptable] is the type of a possible secret that can be stored *)

(** [encrypted_form] is the type indicating whether the encrypted data is a
    login or a password. *)
type encrypted_form =
  | EncryptedLogin
  | EncryptedPassword

(** [encrypted] is the type that records all data and metadata associated with
    the encrypted entry. *)
type encrypted =
  | EncryptedString of {
      form : encrypted_form;
      name : string;
      encrypted_data : string;
    }

val name_of_encryptable : encryptable -> string
(** [name_of_encryptable entry] retrieves the name associated with [entry]. *)

val password_of_encryptable : encryptable -> string
(** [password_of_encryptable entry] retrieves the secret password associated
    with [entry]. *)

val string_of_password : password -> string
(** [string_of_password p] is a string representation of [p] for logging or
    debugging. *)

val string_of_login : login -> string
(** [string_of_login l] is a string representation of [l] for logging or
    debugging. *)

val string_of_encryptable : encryptable -> string
(** [string_of_encryptable e] is a string representation of [e] for logging or
    debugging. *)

val string_of_master_password_hash : string -> string
(** [string_of_master_password_hash h] is a string representation of [h] for
    logging or debugging. *)

val string_of_unencryptable : unencryptable -> string
(** [string_of_unencryptable u] is a string representation of [u] for logging or
    debugging. *)
