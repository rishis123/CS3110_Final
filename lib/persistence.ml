let masterpwd_filename =
  "masterpwd" (* Filename for file that stores master password in hashed form*)

let pwd_filename =
  "pwd" (* Filename for file that stores all passwords in hashed form*)

(* Goes to the file with the hashed master password, and returns the first line
   (which is still hashed). Hash is presumably in first line.*)
let read_master_password_hash () =
  let lines =
    BatList.of_enum (BatFile.lines_of ("data/" ^ masterpwd_filename))
  in
  let hash = BatList.hd lines in
  hash

(* Write unencryptable information i.e. master password to memory for first
   time. Presumably passed in in hashed from. *)
let write_unencryptable master_value =
  match master_value with
  | Types.MasterPasswordHash hash ->
      let original =
        BatList.of_enum (BatFile.lines_of ("data/" ^ pwd_filename))
        (* takes whatever passwords are already in the file*)
      in
      let new_stuff = BatList.enum (BatList.cons hash original) in
      BatFile.write_lines ("data/" ^ masterpwd_filename) new_stuff

(* Revised functionality to include either password or login values *)
let read_all_encryptable () =
  let lines = BatList.of_enum (BatFile.lines_of ("data/" ^ pwd_filename)) in
  BatList.map (fun x -> Encrypt.decrypt_password x) lines
(* NOTE: THIS WILL BE MODIFIED WHEN decrypt_login FUNCTIONALITY IS DONE*)

(* Writes either password or login information to file*)
let write_encryptable encryptable =
  match encryptable with
  | Types.Password _ ->
      let line = Encrypt.encrypt encryptable in
      let original =
        BatList.of_enum (BatFile.lines_of ("data/" ^ pwd_filename))
        (* takes whatever passwords are already in the file*)
      in
      let new_stuff = BatList.enum (BatList.cons line original) in
      BatFile.write_lines ("data/" ^ pwd_filename) new_stuff
      (* Prepends the new password we want to encrypt and write to file with
         everything already in the passwords file, then writes everything to
         memory*)
      (* *)
  | Types.Login _ -> failwith "Not implemented yet"

(* Given the password or login we want to delete in unencrypted -- first
   encrypts them (assuming encryption function always yields the same output).
   Then, searches the BatFile for it, and removes it.*)
let delete_encryptable_by_name encrypt_val_name =
  let encryptable_list = read_all_encryptable () in
  let filtered_list =
    List.filter
      (fun encryptable ->
        match encryptable with
        | Types.Password pwd -> pwd.name <> encrypt_val_name
        | Types.Login login -> login.name <> encrypt_val_name)
      encryptable_list
  in
  let encrypted_filtered_list = List.map Encrypt.encrypt filtered_list in
  BatFile.write_lines ("data/" ^ pwd_filename)
    (BatList.enum encrypted_filtered_list)

let autocomplete _ = raise (Failure "Not implemented")
