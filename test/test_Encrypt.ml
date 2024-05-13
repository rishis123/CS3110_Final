open OUnit2
open FinalProject.Types
open FinalProject.Encrypt
open TestUtil

let key1 = "password123"
let key2 = "skibidi dop dop dop yes yes"

(** [substring s1 s2] is [true] is [s2] is a substring of [s1], and [false]
    otherwise. *)
let rec substring s1 s2 =
  if String.length s1 < String.length s2 then false
  else
    let s2_len = String.length s2 in
    if String.sub s1 0 s2_len = s2 then true
    else substring (String.sub s1 1 (String.length s1 - 1)) s2

let substring_deterministic_tests =
  [
    ( "banana contains ana" >:: fun _ ->
      substring "banana" "ana" |> assert_equal true );
    ( "banana does not contain nanan" >:: fun _ ->
      substring "banana" "nanan" |> assert_equal false );
    ( "keysmash substring" >:: fun _ ->
      substring "]q-4tqopyhio[erphun4589qrewknqe" "4589qrewk"
      |> assert_equal true );
    ( "keysmash non-substring" >:: fun _ ->
      substring "2tu92-5 [o35yu-294igj2]itj[5h33" "ylk54jh3r]"
      |> assert_equal false );
  ]

let substring_randomized_tests =
  let open QCheck in
  [
    Test.make ~name:"A string is a substring of itself." ~count:200 string
      (fun str -> substring str str = true);
    Test.make ~name:"The empty string is a substring of any string." ~count:200
      string (fun str -> substring str "" = true);
    Test.make
      ~name:
        "If s2 is strictly longer than s1, then s2 cannot be a substring of s1."
      ~count:200
      (pair small_string (string_of_size (Gen.int_range 11 10000)))
      (fun (s1, s2) -> substring s1 s2 = false);
    Test.make ~name:"If s2 is a prefix of s1, then s2 is a substring of s1."
      ~count:200 (pair string string) (fun (s1, s2) ->
        substring (s2 ^ s1) s2 = true);
    Test.make ~name:"If s2 is a suffix of s1, then s2 is a substring of s1."
      ~count:200 (pair string string) (fun (s1, s2) ->
        substring (s1 ^ s2) s2 = true);
  ]

let randomized_tests =
  let open QCheck in
  [
    Test.make ~name:"Dec(key, Enc(key, password)) is password." ~count:200
      password_arb (fun pwd ->
        set_key key1;
        decrypt (encrypt (Password pwd)) = Password pwd);
    Test.make ~name:"Dec(key, Enc(key, login)) is login." ~count:200 login_arb
      (fun lgn ->
        set_key key1;
        decrypt (encrypt (Login lgn)) = Login lgn);
    Test.make
      ~name:
        "If key1 <> key2, then Dec(key2, Enc(key1, password)) should not be \
         password, raise bad padding, or raise json error."
      ~count:200 password_arb (fun pwd ->
        set_key key1;
        let enc = encrypt (Password pwd) in
        set_key key2;
        try decrypt enc <> Password pwd
        with Cryptokit.Error Bad_padding | Yojson__Common.Json_error _ -> true);
    Test.make
      ~name:
        "If key1 <> key2, then Dec(key2, Enc(key1, login)) should not be \
         login, raise bad padding, or raise json error." ~count:200 login_arb
      (fun lgn ->
        set_key key1;
        let enc = encrypt (Login lgn) in
        set_key key2;
        try decrypt enc <> Login lgn
        with Cryptokit.Error Bad_padding | Yojson__Common.Json_error _ -> true);
    Test.make
      ~name:
        "The encrypted data in Enc(key, password)) does not contain any of the \
         fields in password as a substring, given the fields contain data of \
         length at least 10." ~count:200 password_arb (fun pwd ->
        set_key key1;
        match encrypt (Password pwd) with
        | EncryptedString es ->
            let ciphertext = String.uppercase_ascii es.encrypted_data in
            (not (substring ciphertext (String.uppercase_ascii pwd.name)))
            && not (substring ciphertext (String.uppercase_ascii pwd.password)));
    Test.make
      ~name:
        "The encrypted data in Enc(key, login)) does not contain of the fields \
         in login as a substring, given the fields contain data of length at \
         least 10." ~count:200 login_arb (fun lgn ->
        set_key key1;
        match encrypt (Login lgn) with
        | EncryptedString es -> (
            let ciphertext = String.uppercase_ascii es.encrypted_data in
            (not (substring ciphertext (String.uppercase_ascii lgn.name)))
            && (not
                  (substring ciphertext (String.uppercase_ascii lgn.username)))
            && (not
                  (substring ciphertext (String.uppercase_ascii lgn.password)))
            &&
            match lgn.url with
            | Some url ->
                not (substring ciphertext (String.uppercase_ascii url))
            | None -> true));
  ]

let encrypt_test_suite =
  "encrypt test suite"
  >::: substring_deterministic_tests
       @ QCheck_runner.to_ounit2_test_list substring_randomized_tests
       @ QCheck_runner.to_ounit2_test_list randomized_tests
