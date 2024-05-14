let string_to_salted_hash str = Types.MasterPasswordHash (Bcrypt.hash str)

let string_to_sha3_hash str =
  let hash_function = Cryptokit.Hash.sha3 256 in
  (* 256-bit SHA3 hash *)
  hash_function#add_string str;
  hash_function#result

let check_master_pwd password =
  (* let password1 = string_to_salted_hash password in let string_of_pwd1 =
     Bcrypt.string_of_hash password1 in *)
  let filename = "data/unencrypted/masterpwd" in
  let ic = open_in filename in
  try
    let stored_password = input_line ic in
    let hash_of_stored_pwd = Bcrypt.hash_of_string stored_password in

    close_in ic;

    if Bcrypt.verify password hash_of_stored_pwd then true else false
  with End_of_file ->
    close_in ic;
    false

(* let check_master password = (* let password1 = string_to_sha3_hash password
   in print_endline password1; *) let filename = "data/unencrypted/masterpwd" in
   let ic = open_in filename in try let stored_password = input_line ic in
   print_endline stored_password; close_in ic;

   if password = stored_password then true else false with End_of_file ->
   close_in ic; false *)
