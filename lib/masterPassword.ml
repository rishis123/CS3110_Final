let string_to_sha3_hash str =
  let hash_function = Cryptokit.Hash.sha3 256 in
  (* 256-bit SHA3 hash *)
  hash_function#add_string str;
  hash_function#result

let check_master password =
  let password1 = string_to_sha3_hash password in
  let filename = "data/unencrypted/masterpwd" in
  let ic = open_in filename in
  try
    let stored_password = input_line ic in
    close_in ic;

    if password1 = stored_password then true else false
  with End_of_file ->
    close_in ic;
    false
