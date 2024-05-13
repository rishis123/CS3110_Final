open OUnit2
open FinalProject

(* Note: reproduce functions in persistence -- writing to these test files
   instead.*)
let masterpwd_file_path = "data/unencrypted/test_unencryptables"
let encrypted_file_path = "data/encrypted/test_encryptables"
let permission = 0o777

module Dirs = struct
  let masterpwd_file_path = masterpwd_file_path
  let encrypted_file_path = encrypted_file_path
  let permission = 0o777
end

module Persistence = Persistence.Make (Dirs)

let set_file_perms () =
  Encrypt.set_key "password123";

  (* Ensure directories exist *)
  if not (Sys.file_exists (Filename.dirname masterpwd_file_path)) then
    Unix.mkdir (Filename.dirname masterpwd_file_path) 0o777;
  if not (Sys.file_exists (Filename.dirname encrypted_file_path)) then
    Unix.mkdir (Filename.dirname encrypted_file_path) 0o777;
  Unix.chmod masterpwd_file_path 0o777;
  Unix.chmod encrypted_file_path 0o777

(* Note: below are same functions as persistence, but we want to use the above
   file path for testing rather than modify the same ones for the users.*)
let read_all_encryptable_seq () =
  Yojson.Basic.seq_from_file ~fname:encrypted_file_path encrypted_file_path
  |> Seq.map Serialization.encrypted_of_json
  |> Seq.map Encrypt.decrypt

let read_all_encryptable () = read_all_encryptable_seq () |> List.of_seq

let write_encryptable encryptable =
  let old_entries =
    Yojson.Basic.seq_from_file ~fname:encrypted_file_path encrypted_file_path
  in
  let new_entry =
    Encrypt.encrypt encryptable |> Serialization.json_of_encrypted
  in
  let new_entries = Seq.cons new_entry old_entries in
  let new_entries_copy = new_entries |> List.of_seq |> List.to_seq in
  Yojson.Basic.seq_to_file encrypted_file_path new_entries_copy

let tests =
  [
    ( "Test write encryptable (password) and read it" >:: fun _ ->
      (* set_file_perms (); *)
      let try_pass = Types.Password { name = "monkey"; password = "abc123" } in
      let () = write_encryptable try_pass in
      let mem_list = read_all_encryptable () in
      let encrypt_elem = List.hd mem_list in
      assert_equal "P{ name = monkey; password = abc123 }"
        (Types.string_of_encryptable encrypt_elem) );
    ( "Test add a login as well and read both" >:: fun _ ->
      set_file_perms ();
      let try_login =
        Types.Login
          {
            name = "monkey";
            username = "donkey";
            password = "abc123";
            url = Some "joe";
          }
      in
      let () = write_encryptable try_login in
      let mem_list = read_all_encryptable () in
      let encrypt_second = List.hd mem_list in
      (* the password*)
      let encrypt_first = List.hd (List.tl mem_list) in
      (*the login*)
      assert_equal "P{ name = monkey; password = abc123 }"
        (Types.string_of_encryptable encrypt_first);
      assert_equal
        "L{ name = monkey; username = donkey; password = abc123; url = joe }"
        (Types.string_of_encryptable encrypt_second) );
  ]

let persistence_suite = "persistence test suite" >::: tests
