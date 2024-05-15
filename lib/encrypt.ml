open Cryptokit
open Cipher
open Hash
open Types

(** [masterkey] is [ref None] if and only if [set_key] has not yet been called. *)
let masterkey : string option ref = ref None

let set_key key = masterkey := Some key

(** [form_of_encryptable] determines if the encryptable is a login or a
    password. *)
let form_of_encryptable = function
  | Types.Login _ -> EncryptedLogin
  | Types.Password _ -> EncryptedPassword

(** [string_to_sha2_hash str] is the 256-bit SHA2 hash of [str]. *)
let string_to_sha2_hash str =
  let hash_function = sha256 () in
  (* 256-bit SHA3 hash *)
  hash_function#add_string str;
  hash_function#result

(** [transform direction target] is the ciphertext of [target] if [direction] is
    [Cipher.Encrypt], and is the plaintext of [target] if [direction] is
    [Cipher.Decrypt]. Currently, this method implements encryption/decryption
    using AES256, with the key being the SHA256 hash of the masterkey. *)
let transform direction target =
  match !masterkey with
  | None -> failwith "Masterkey not yet set!"
  | Some masterkey ->
      let key = string_to_sha2_hash masterkey in
      (* key derivation of [masterkey] *)
      let transformer = aes ~pad:Padding.length key direction in
      transformer#put_string target;
      transformer#finish;
      transformer#get_string

let encrypt encryptable =
  let encrypted_data =
    transform Encrypt (Serialization.encryptable_to_string encryptable)
  in
  EncryptedString
    {
      form = form_of_encryptable encryptable;
      name = name_of_encryptable encryptable;
      encrypted_data;
    }

(** [decrypt_password encrypted_data] is the password obtained from decrypting
    [encrypted_data]. *)
let decrypt_password encrypted_data =
  (* Types.Password { name = ""; password = encrypted } *)
  let decrypted_data = transform Decrypt encrypted_data in
  match Serialization.encryptable_of_string_opt decrypted_data with
  | Some (Password password) -> password
  | _ -> failwith "Invalid encrypted password"

(** [decrypt_password encrypted_data] is the login obtained from decrypting
    [encrypted_data]. *)
let decrypt_login encrypted_data =
  let decrypted_data = transform Decrypt encrypted_data in
  match Serialization.encryptable_of_string_opt decrypted_data with
  | Some (Login login) -> login
  | _ -> failwith "Invalid encrypted login"

let decrypt (EncryptedString { form; encrypted_data; _ }) =
  match form with
  | EncryptedPassword -> Types.Password (decrypt_password encrypted_data)
  | EncryptedLogin -> Types.Login (decrypt_login encrypted_data)
