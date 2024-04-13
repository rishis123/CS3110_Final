let masterkey = ref ""
let set_key key = masterkey := key
let encrypt encryptable = Serialization.encryptable_to_string encryptable

let decrypt_password encrypted =
  Types.Password { name = ""; password = encrypted }

let decrypt_login encrypted =
  ignore encrypted;
  failwith "Not implemented yet"

let salt_hash pwd = Types.MasterPasswordHash pwd
