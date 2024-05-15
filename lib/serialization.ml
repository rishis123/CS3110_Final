open Types

let string_option_to_json = function
  | Some s -> `String s
  | None -> `Null

let json_to_string_option : Yojson.Basic.t -> string option = function
  | `Null -> None
  | `String s -> Some s
  | _ -> failwith "Invalid JSON type for conversion to string option"

let unencryptable_to_json (MasterPasswordHash hash) : Yojson.Basic.t =
  `Assoc [ ("master_password_hash", `String (Bcrypt.string_of_hash hash)) ]

let unencryptable_to_string h =
  h |> unencryptable_to_json |> Yojson.Basic.to_string

let unencryptable_of_string_opt str =
  match Yojson.Basic.from_string str with
  (* PATTERN MUST MATCH RETURN OF unencryptable_to_json !! *)
  | `Assoc [ ("master_password_hash", `String hash) ] ->
      Some (MasterPasswordHash (Bcrypt.hash_of_string hash))
  | _ -> None

let password_to_json ({ name; password } : password) : Yojson.Basic.t =
  `Assoc [ ("name", `String name); ("password", `String password) ]

let login_to_json { name; username; password; url } : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String name);
      ("username", `String username);
      ("password", `String password);
      ("url", string_option_to_json url);
    ]

let encryptable_to_json = function
  | Password p -> password_to_json p
  | Login l -> login_to_json l

let encryptable_to_string enc =
  enc |> encryptable_to_json |> Yojson.Basic.to_string

let encryptable_of_string_opt str =
  match Yojson.Basic.from_string str with
  (* PATTERNS MUST MATCH RETURNS OF login_to_json AND encryptable_to_json !!*)
  | `Assoc
      [
        ("name", `String name);
        ("username", `String username);
        ("password", `String password);
        ("url", url_or_null);
      ] ->
      Some
        (Login
           { name; username; password; url = json_to_string_option url_or_null })
  | `Assoc [ ("name", `String name); ("password", `String password) ] ->
      Some (Password { name; password })
  | _ -> None

let json_of_encrypted_form : encrypted_form -> Yojson.Basic.t = function
  | EncryptedPassword -> `String "password"
  | EncryptedLogin -> `String "login"

let encrypted_form_of_json : Yojson.Basic.t -> encrypted_form = function
  | `String "password" -> EncryptedPassword
  | `String "login" -> EncryptedLogin
  | _ -> failwith "Invalid form"

let json_of_encrypted (EncryptedString { form; name; encrypted_data }) :
    Yojson.Basic.t =
  `Assoc
    [
      ("form", json_of_encrypted_form form);
      ("name", `String name);
      ("encrypted_data", `String encrypted_data);
    ]

let encrypted_of_json (json : Yojson.Basic.t) =
  match json with
  | `Assoc
      [
        ("form", form);
        ("name", `String name);
        ("encrypted_data", `String encrypted_data);
      ] ->
      EncryptedString
        { form = encrypted_form_of_json form; name; encrypted_data }
  | _ -> failwith "Cannot parse into encrypted; invalid format"
