let check_master password =
  let filename = "data/unencrypted/masterpwd" in
  let ic = open_in filename in
  try
    let stored_password = input_line ic in
    close_in ic;
    if password = stored_password then true else false
  with End_of_file ->
    close_in ic;
    false
