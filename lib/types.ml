type master_password_hash = string
type unencryptable = MasterPasswordHash of master_password_hash

type password = {
  name : string;
  password : string;
}

type login = {
  name : string;
  username : string;
  password : string;
  url : string option;
}

(* User has choice to encrypt either whole password, or login info*)
type encryptable =
  | Password of password
  | Login of login

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
