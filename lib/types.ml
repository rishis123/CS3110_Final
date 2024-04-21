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
