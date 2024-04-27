open Types

let string_option_to_json = function
  | Some s -> `String s
  | None -> `Null

let json_to_string_option : Yojson.Basic.t -> string option = function
  | `Null -> None
  | `String s -> Some s
  | _ -> failwith "Invalid JSON type for conversion to string option"

let unencryptable_to_json (MasterPasswordHash hash) : Yojson.Basic.t =
  `Assoc [ ("master_password_hash", `String hash) ]

let unencryptable_to_string h =
  h |> unencryptable_to_json |> Yojson.Basic.to_string

let unencryptable_of_string_opt str =
  match Yojson.Basic.from_string str with
  (* PATTERN MUST MATCH RETURN OF unencryptable_to_json !! *)
  | `Assoc [ ("master_password_hash", `String hash) ] ->
      Some (MasterPasswordHash hash)
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
