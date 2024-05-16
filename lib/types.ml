type master_password_hash = Bcrypt.hash
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

type encryptable =
  | Password of password
  | Login of login

type encrypted_form =
  | EncryptedLogin
  | EncryptedPassword

type encrypted =
  | EncryptedString of {
      form : encrypted_form;
      name : string;
      encrypted_data : string;
    }

let name_of_encryptable = function
  | Password { name; _ } -> name
  | Login { name; _ } -> name

let password_of_encryptable = function
  | Password { password; _ } -> password
  | Login { password; _ } -> password

let string_of_password (p : password) : string =
  Printf.sprintf "P{ name = %s; password = %s }" p.name p.password

let string_of_login (l : login) : string =
  match l.url with
  | None ->
      Printf.sprintf "L{ name = %s; username = %s; password = %s; [no url] }"
        l.name l.username l.password
  | Some url ->
      Printf.sprintf "L{ name = %s; username = %s; password = %s; url = %s }"
        l.name l.username l.password url

let string_of_encryptable = function
  | Password p -> string_of_password p
  | Login l -> string_of_login l

let string_of_master_password_hash h : string = h

let string_of_unencryptable = function
  | MasterPasswordHash h -> Bcrypt.string_of_hash h
