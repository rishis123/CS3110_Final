open Encryption

(* Filename for file that stores master password in hashed form *)
let masterpwd_file_path = "data/unencrypted/masterpwd"

(** Filename for file that stores all passwords in hashed form*)
let pwd_file_path = "data/encrypted/pwd"

(** [set_file_perms] sets the data files to read and write allowed only for the
    owner. *)
let set_file_perms () =
  Unix.chmod masterpwd_file_path 0o600;
  Unix.chmod pwd_file_path 0o600

(* Precondition: the hash is in the first line of [masterpwd_file_path]. *)
let read_master_password_hash () =
  let lines = BatList.of_enum (BatFile.lines_of masterpwd_file_path) in
  let hash = BatList.hd lines in
  hash

(* Write unencryptable information i.e. master password to memory for first
   time. Presumably passed in in hashed from. *)
let write_unencryptable master_value =
  match master_value with
  | Types.MasterPasswordHash hash ->
      BatFile.write_lines masterpwd_file_path (BatList.enum [ hash ])

(* Revised functionality to include either password or login values *)
let read_all_encryptable () =
  let lines = BatList.of_enum (BatFile.lines_of pwd_file_path) in
  BatList.map
    (fun x -> Types.Password (Encrypt.decrypt_password (EncryptedString x)))
    lines
(* NOTE: THIS WILL BE MODIFIED WHEN decrypt_login FUNCTIONALITY IS DONE*)

(* Writes either password or login information to file*)
let write_encryptable encryptable =
  match encryptable with
  | Types.Password _ ->
      let (EncryptedString line) = Encrypt.encrypt encryptable in
      let original =
        BatList.of_enum (BatFile.lines_of pwd_file_path)
        (* takes whatever passwords are already in the file*)
      in
      let new_stuff = BatList.enum (BatList.cons line original) in
      BatFile.write_lines pwd_file_path new_stuff
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
  let encrypted_filtered_lines =
    List.map (fun (Encrypt.EncryptedString str) -> str) encrypted_filtered_list
  in
  BatFile.write_lines pwd_file_path (BatList.enum encrypted_filtered_lines)
