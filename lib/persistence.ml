open Types

(* Filename for file that stores master password in hashed form *)
let masterpwd_file_path = "data/unencrypted/masterpwd"

(** Filename for file that stores all encrypted data. Invariant: this data
    matches the schema in schemas/encryptables-schema.json *)
let encrypted_file_path = "data/encrypted/encryptables"

(** [set_file_perms] sets the data files to read and write allowed only for the
    owner. *)
let set_file_perms () =
  Unix.chmod masterpwd_file_path 0o600;
  Unix.chmod encrypted_file_path 0o600

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

let read_all_encryptable_seq () =
  Yojson.Basic.seq_from_file ~fname:encrypted_file_path encrypted_file_path
  |> Seq.map Serialization.encrypted_of_json
  |> Seq.map Encrypt.decrypt

let read_all_encryptable () = read_all_encryptable_seq () |> List.of_seq

(* Writes either password or login information to file*)
let write_encryptable encryptable =
  let old_entries =
    Yojson.Basic.seq_from_file ~fname:encrypted_file_path encrypted_file_path
  in
  let new_entry =
    Encrypt.encrypt encryptable |> Serialization.json_of_encrypted
    (* takes whatever passwords are already in the file*)
  in
  let new_entries = Seq.cons new_entry old_entries in
  (* Must read the data fully into memory before writing it since otherwise
     reading and writing will occur concurrently leading to data corruption *)
  let new_entries_copy = new_entries |> List.of_seq |> List.to_seq in
  (* Prepends the new password we want to encrypt and write to file with
     everything already in the passwords file, then writes everything to
     memory*)
  Yojson.Basic.seq_to_file encrypted_file_path new_entries_copy

(* Given the password or login we want to delete in unencrypted -- first
   encrypts them (assuming encryption function always yields the same output).
   Then, searches the BatFile for it, and removes it.*)
let delete_encryptable_by_name encrypt_val_name =
  let encryptable_seq = read_all_encryptable_seq () in
  let filtered_seq =
    Seq.filter
      (fun encryptable -> name_of_encryptable encryptable = encrypt_val_name)
      encryptable_seq
  in
  let encrypted_filtered_seq = Seq.map Encrypt.encrypt filtered_seq in
  let encrypted_filtered_lines =
    Seq.map Serialization.json_of_encrypted encrypted_filtered_seq
  in
  Yojson.Basic.seq_to_file encrypted_file_path encrypted_filtered_lines
