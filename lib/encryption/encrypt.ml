type encrypted = EncryptedString of string

let masterkey = ref ""
let set_key key = masterkey := key

let encrypt encryptable =
  EncryptedString (Serialization.encryptable_to_string encryptable)

let decrypt_password (EncryptedString encrypted) =
  match Serialization.encryptable_of_string_opt encrypted with
  | Some (Password password) -> password
  | _ -> failwith "Invalid encrypted password"

let decrypt_login (EncryptedString encrypted) =
  ignore encrypted;
  failwith "Not implemented yet"

let salt_hash pwd = Types.MasterPasswordHash pwd
