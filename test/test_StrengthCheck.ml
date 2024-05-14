open OUnit2

let tests =
  [
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
    ( "Test vulnerability checker (list of login/pwd)" >:: fun _ ->
      let sample_data =
        [
          Types.Password { name = "monkey"; password = "abc123" };
          Types.Password { name = "donkey"; password = "man" };
          Types.Password { name = "conkey"; password = "r0is1hehsa8sh" };
          Types.Password { name = "lonkey"; password = "123abc" };
          Types.Login
            {
              name = "fonkey";
              username = "user1";
              password = "taylorswift123";
              url = None;
            };
          Types.Login
            {
              name = "ronkey";
              username = "user2";
              password = "$#*(*@_)";
              url = Some "certifiedloverboy";
            };
          Types.Login
            {
              name = "tonkey";
              username = "user3";
              password = "@*(@*)";
              url = None;
            };
        ]
      in
      let vulner_lst = fake_check_vulner sample_data in
      let expect_lst = [ "monkey"; "donkey"; "lonkey"; "fonkey" ] in

      let sorted_vulner_lst = List.sort compare vulner_lst in
      let sorted_expect_lst = List.sort compare expect_lst in

      assert_equal sorted_expect_lst sorted_vulner_lst );
  ]
