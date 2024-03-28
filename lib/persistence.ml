let masterpwd_filename = "masterpwd"
let pwd_filename = "pwd"

let read_master_password_hash () =
  let lines =
    BatList.of_enum (BatFile.lines_of ("data/" ^ masterpwd_filename))
  in
  let hash = BatList.hd lines in
  Types.MasterPasswordHash hash

let write_unencryptable _ = failwith "Not implemented"

let read_all_encryptable () =
  let lines =
    BatList.of_enum (BatFile.lines_of ("data/" ^ masterpwd_filename))
  in
  BatList.map (fun x -> Types.Password { name = ""; password = x }) lines

let write_encryptable encryptable =
  match encryptable with
  | Types.Password _ ->
      let line = Serialization.encryptable_to_string encryptable in
      let oc = open_out ("data/" ^ pwd_filename) in
      Printf.fprintf oc "%s" line;
      close_out oc
