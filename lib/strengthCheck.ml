let common_passwords = ref (fun () -> failwith "Not yet initialized")

let init () =
  let pass = Trie.of_file "data/xato-net-10-million-passwords.txt" in
  common_passwords := (fun () -> pass)

let is_weak password_str =
  Trie.mem (password_str) (!common_passwords ())