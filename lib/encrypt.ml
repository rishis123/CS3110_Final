open Types

let masterkey = ref ""
let set_key key = masterkey := key

let form_of_encryptable = function
  | Types.Login _ -> EncryptedLogin
  | Types.Password _ -> EncryptedPassword

let encrypt encryptable =
  EncryptedString
    {
      form = form_of_encryptable encryptable;
      name = name_of_encryptable encryptable;
      encrypted_data =
        "encrypted 😎: " ^ Serialization.encryptable_to_string encryptable;
    }

let decrypt_password encrypted_data =
  match Serialization.encryptable_of_string_opt encrypted_data with
  | Some (Password password) -> password
  | _ -> failwith "Invalid encrypted password"

let decrypt_login encrypted_data =
  match Serialization.encryptable_of_string_opt encrypted_data with
  | Some (Login login) -> login
  | _ -> failwith "Invalid encrypted password"

let decrypt (EncryptedString { form; encrypted_data; _ }) =
  match form with
  | EncryptedPassword -> Types.Password (decrypt_password encrypted_data)
  | EncryptedLogin -> Types.Login (decrypt_login encrypted_data)

let salt_hash pwd = Types.MasterPasswordHash pwd
