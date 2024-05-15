open OUnit2
open FinalProject

(* Makes a string of 3 to 10 random characters, any printable ASCII allowed*)
let generate_random_string () =
  let random_char () = char_of_int (Random.int 94 + 32) in
  let length = Random.int 8 + 3 in
  (* 3 to 10 characters long*)
  String.init length (fun _ -> random_char ())

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
    ( "Test compare words" >:: fun _ ->
      (* test non-overlapping strings*)
      let all_good = ref true in
      for _ = 0 to 10 do
        let init_str = generate_random_string () in
        let fin_str = modify_string init_str in
        if Autocomplete.compare_words init_str fin_str then all_good := false
      done;
      assert_bool "Finds overlap of 3 when cannot exist" !all_good );
    ( "Test autocomplete for logins and passwords " >:: fun _ ->
      let sample_data =
        [
          Types.Password { name = "password1"; password = "abc123" };
          Types.Password { name = "monk"; password = "xyz456" };
          Types.Password { name = "pas"; password = "123abc" };
          Types.Password { name = "donk"; password = "123abc" };
          Types.Login
            {
              name = "login1";
              username = "user1";
              password = "pass1";
              url = None;
            };
          Types.Login
            {
              name = "pas";
              username = "user2";
              password = "pass2";
              url = Some "monkey";
            };
          Types.Login
            {
              name = "chass";
              username = "user3";
              password = "pass3";
              url = None;
            };
        ]
      in
      (* Mocking the Persistence.read_all_encryptable function *)
      let filtered_list = fake_autocomplete sample_data "pass" in
      (* Assert that only passwords or logins with names containing at least 3
         characters in common with "pass" are present *)
      let expected_filtered_list =
        [
          Types.Password { name = "password1"; password = "abc123" };
          Types.Password { name = "pas"; password = "123abc" };
          Types.Login
            {
              name = "pas";
              username = "user2";
              password = "pass2";
              url = Some "monkey";
            };
          Types.Login
            {
              name = "chass";
              username = "user3";
              password = "pass3";
              url = None;
            };
        ]
      in
      assert_equal expected_filtered_list filtered_list );
  ]

let autocomplete_suite = "autocomplete test suite" >::: tests
