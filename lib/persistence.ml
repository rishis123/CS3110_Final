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

(** Function to remove all non-alphanumeric characters from the word [str], and
    make it lowercase. *)
let clean_up str =
  let normalized = String.lowercase_ascii str in
  let is_alphanumeric c =
    match c with
    | 'a' .. 'z' | '0' .. '9' -> true
    | _ -> false
  in
  let rec clean_chars index =
    if index >= String.length normalized then ""
    else if is_alphanumeric normalized.[index] then
      String.make 1 normalized.[index] ^ clean_chars (index + 1)
    else clean_chars (index + 1)
  in
  clean_chars 0

(** Function that returns true if [str1] and [str2] have at least 3 characters
    in common. Note that this is not case-sensitive, and doesn't include
    non-alphanumerics in names.*)
let compare_words str1 str2 =
  let str1 = clean_up str1 in
  let str2 = clean_up str2 in
  let len1 = String.length str1 in
  let len2 = String.length str2 in
  let rec check_overlap i1 i2 count =
    if i1 >= len1 || i2 >= len2 then count >= 3
    else if str1.[i1] = str2.[i2] then
      check_overlap (i1 + 1) (i2 + 1) (count + 1)
    else check_overlap (i1 + 1) 0 0
  in
  let rec iterate i1 =
    if i1 >= len1 then false
    else
      let rec iterate_inner i2 =
        if i2 >= len2 then iterate (i1 + 1)
        else if check_overlap i1 i2 0 then true
        else iterate_inner (i2 + 1)
      in
      iterate_inner 0
  in
  iterate 0

(** Returns Type.encryptable list representing every value in the saved
    passwords/login file with a name sharing at least 3 characters in common
    with [seek_word]*)
let autocomplete seek_word =
  let encryptable_list = read_all_encryptable () in
  let filtered_list =
    List.filter
      (fun encryptable ->
        match encryptable with
        | Types.Password pwd -> compare_words pwd.name seek_word
        | Types.Login login -> compare_words login.name seek_word)
      encryptable_list
  in
  filtered_list
