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
  let lines = BatList.of_enum (BatFile.lines_of ("data/" ^ pwd_filename)) in
  BatList.map (fun x -> Encrypt.decrypt_password x) lines

let write_encryptable encryptable =
  match encryptable with
  | Types.Password _ ->
      let line = Encrypt.encrypt encryptable in
      let original =
        BatList.of_enum (BatFile.lines_of ("data/" ^ pwd_filename))
      in
      let new_stuff = BatList.enum (BatList.cons line original) in
      BatFile.write_lines ("data/" ^ pwd_filename) new_stuff
  | Types.Login _ -> failwith "Not implemented yet"