open OUnit2
open FinalProject

let pick_10_in_common () =
  let lst_of_passwords = Autocomplete.common_passwords in
  (* list of 100 most commmon passwords*)
  let len = List.length lst_of_passwords in
  (* needed in picking a random element each time *)
  let output_lst = ref [] in

  for _ = 0 to 9 do
    let random_index = Random.int len in
    let password = List.nth lst_of_passwords random_index in
    (* pick a random password in the list of 100 commons *)
    let len_pass = String.length password in
    let start_index = Random.int (len_pass - 2) in
    (*start at this index, must be 3 from end -- build onwards *)
    let output = String.sub password start_index 3 in
    output_lst := output :: !output_lst
  done;
  !output_lst

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
    ( "Test check strength" >:: fun _ ->
      let ten_random = pick_10_in_common () in
      let all_valid = ref true in
      (* problematic if this is false*)
      for i = 0 to 9 do
        (* true means an overlap exists*)
        if not (Autocomplete.check_strength (List.nth ten_random i)) then
          all_valid := false
      done;
      assert_bool "Check strength finds no overlap between overlappers"
        !all_valid );
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
