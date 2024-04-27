type master_password_hash = string
(** [master_password_hash] is the type of a hash of the master password, used
    for checking validity at startup *)

(** [unencryptable] is the type of any storable data that is not encrypted *)
type unencryptable = MasterPasswordHash of master_password_hash

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

(** [encryptable] is the type of a possible secret that can be stored *)
type encryptable =
  | Password of password
  | Login of login

let name_of_encryptable = function
  | Password { name; _ } -> name
  | Login { name; _ } -> name

(** [string_of_password p] is a string representation of [p] for logging or
    debugging. *)
let string_of_password (p : password) : string =
  Printf.sprintf "P{ name=%s; password=%s }" p.name p.password

(** [string_of_login l] is a string representation of [l] for logging or
    debugging. *)
let string_of_login (l : login) : string =
  match l.url with
  | None ->
      Printf.sprintf "L{ name=%s; username=%s; password=%s; [no url] }" l.name
        l.username l.password
  | Some url ->
      Printf.sprintf "L{ name=%s; username=%s; password=%s; url=%s }" l.name
        l.username l.password url

(** [string_of_encryptable e] is a string representation of [e] for logging or
    debugging. *)
let string_of_encryptable : encryptable -> string = function
  | Password p -> string_of_password p
  | Login l -> string_of_login l

(** [string_of_master_password_hash h] is a string representation of [h] for
    logging or debugging. *)
let string_of_master_password_hash (h : master_password_hash) : string = h

(** [string_of_unencryptable u] is a string representation of [u] for logging or
    debugging. *)
let string_of_unencryptable : unencryptable -> string = function
  | MasterPasswordHash h -> string_of_master_password_hash h
