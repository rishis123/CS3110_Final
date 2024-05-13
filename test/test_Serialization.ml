open OUnit2
open FinalProject.Types
open FinalProject.Serialization
open TestUtil

let deterministic_tests =
  [
    ( "serialization on password with commas is reversible" >:: fun _ ->
      let pwd =
        Password
          {
            name = "name, with commas!";
            password = "Surely, and hopefully, this will work!";
          }
      in
      encryptable_of_string_opt (encryptable_to_string pwd)
      |> assert_equal (Some pwd) );
    ( "serialization on login with commas is reversible" >:: fun _ ->
      let lgn =
        Login
          {
            name = "name, with commas!";
            username =
              "honk, honk honk honk. coo coo? chickadee, dee, dee, dee.";
            password = "Surely, and hopefully, this will work!";
            url =
              Some
                "https://example.com/product?filter_color:blue,green&filter_size:xl%2Cxxl";
          }
      in
      encryptable_of_string_opt (encryptable_to_string lgn)
      |> assert_equal (Some lgn) );
  ]

let randomized_tests =
  let open QCheck in
  [
    (* ( "doc example" >:: fun _ -> assert_equal (encryptable_to_string
       (Password { name = "Beans"; password = "pwd123" })) "Beans,pwd123" ); *)
    Test.make
      ~name:
        "encryptable_of_string_opt is a left inverse of encryptable_to_string \
         for passwords." ~count:200 password_arb (fun pwd ->
        encryptable_of_string_opt (encryptable_to_string (Password pwd))
        = Some (Password pwd));
    Test.make
      ~name:
        "encryptable_of_string_opt is a left inverse of encryptable_to_string \
         for logins." ~count:200 login_arb (fun lgn ->
        encryptable_of_string_opt (encryptable_to_string (Login lgn))
        = Some (Login lgn));
    (* Test.make ~name: "encryptable_of_string_opt of a string that does not
       represent an \ encryptable is None." ~count:200 string (fun str -> assume
       (str <> ""); encryptable_of_string_opt str = None); *)
  ]

let serialization_suite =
  "serialization test suite"
  >::: deterministic_tests @ QCheck_runner.to_ounit2_test_list randomized_tests
