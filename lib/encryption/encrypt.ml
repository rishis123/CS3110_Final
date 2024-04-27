open Types

type encrypted =
  | EncryptedString of {
      name : string;
      encrypted_data : string;
    }

let masterkey = ref ""
let set_key key = masterkey := key

let encrypt encryptable =
  EncryptedString
    {
      name = name_of_encryptable encryptable;
      encrypted_data = Serialization.encryptable_to_string encryptable;
    }

let decrypt_password (EncryptedString { encrypted_data; _ }) =
  match Serialization.encryptable_of_string_opt encrypted_data with
  | Some (Password password) -> password
  | _ -> failwith "Invalid encrypted password"

let decrypt_login (EncryptedString { name; encrypted_data }) =
  ignore (name, encrypted_data);
  failwith "Not implemented yet"

let salt_hash pwd = Types.MasterPasswordHash pwd
