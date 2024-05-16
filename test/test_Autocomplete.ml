open OUnit2
open FinalProject

(* Makes a string of 3 to 10 random characters, any printable ASCII allowed*)
let generate_random_string () =
  let random_char () = char_of_int (Random.int 94 + 32) in
  let length = Random.int 8 + 3 in
  (* 3 to 10 characters long*)
  String.init length (fun _ -> random_char ())

(* Shifts every character up by 1*)
let modify_string str =
  let char_lst = ref [] in
  for i = 0 to String.length str - 1 do
    let char_val = String.get str i in
    let int_val = Char.code char_val in
    let changed_val = int_val + 1 in
    let output_chr = Char.chr changed_val in
    char_lst := output_chr :: !char_lst
  done;

  String.of_seq (List.to_seq !char_lst)

(* NOTE -- THIS MOCKS Autocomplete.autocomplete function, without the read
   encryptable*)
let fake_autocomplete login_info_lst input_wd =
  let filter_lst =
    List.filter
      (fun encryptable ->
        match encryptable with
        | Types.Password pwd -> Autocomplete.compare_words pwd.name input_wd
        | Types.Login login -> Autocomplete.compare_words login.name input_wd)
      login_info_lst
  in
  filter_lst

let tests =
  [
    ( "Test shouldn't have 3 in common" >:: fun _ ->
      (* test non-overlapping strings*)
      let all_good = ref true in
      for _ = 0 to 10 do
        let init_str = generate_random_string () in
        let fin_str = modify_string init_str in
        if Autocomplete.compare_words init_str fin_str then all_good := false
      done;
      assert_bool "Finds overlap of 3 when cannot exist" !all_good );
    ( "Test should have 3 in common" >:: fun _ ->
      let init_str = GenPassword.generate_password_with_special 10 in
      (* this is just making a random string of length 10 effectively *)
      let short_str = String.sub init_str 0 3 in
      let all_good = ref true in
      if not (Autocomplete.compare_words init_str short_str) then
        all_good := false;
      assert_bool "Doesn't find overlap of 3 when does exist" !all_good );
    ( "Test autocomplete for logins and passwords " >:: fun _ ->
      let name1 = generate_random_string () ^ "pass" in
      let name2 = generate_random_string () in
      let name3 = "pass" ^ generate_random_string () in
      let name4 = "pa" ^ generate_random_string () ^ "s" in
      let name5 =
        generate_random_string () ^ "pas" ^ generate_random_string ()
      in
      let name6 = generate_random_string () in
      let name7 = generate_random_string () in
      let sample_data =
        [
          Types.Password { name = name1; password = "sdsd9" };
          Types.Password { name = name2; password = "xyz456" };
          Types.Password { name = name3; password = "123abc" };
          Types.Password { name = name4; password = "123abc" };
          Types.Login
            { name = name5; username = "user1"; password = "pass1"; url = None };
          Types.Login
            {
              name = name6;
              username = "user2";
              password = "pass2";
              url = Some "monkey";
            };
          Types.Login
            { name = name7; username = "user3"; password = "pass3"; url = None };
        ]
      in
      (* Mocking the Persistence.read_all_encryptable function *)
      let filtered_list = fake_autocomplete sample_data "pass" in
      (* Assert that only passwords or logins with names containing at least 3
         characters in common with "pass" are present *)
      let expected_filtered_list =
        [
          Types.Password { name = name1; password = "sdsd9" };
          Types.Password { name = name3; password = "123abc" };
          Types.Login
            { name = name5; username = "user1"; password = "pass1"; url = None };
        ]
      in

      assert_equal expected_filtered_list filtered_list );
  ]

let autocomplete_suite = "autocomplete test suite" >::: tests
