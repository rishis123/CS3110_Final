open OUnit2
open FinalProject.Types
open FinalProject.Encrypt
open FinalProject.Util
open TestUtil

let key1 = "password123"
let key2 = "skibidi dop dop dop yes yes"

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
  "encrypt test suite" >::: QCheck_runner.to_ounit2_test_list randomized_tests
