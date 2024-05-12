open OUnit2
open FinalProject.Types
open FinalProject.Encrypt

let key1 = "password123"
let key2 = "skibidi dop dop dop yes yes"

let password_arb =
  let open QCheck in
  Gen.map
    (fun (name, password) -> { name; password })
    (Gen.pair Gen.small_string Gen.string)
  |> make

let login_arb =
  let open QCheck in
  Gen.map
    (fun (name, username, password, url) -> { name; username; password; url })
    (Gen.quad Gen.small_string Gen.small_string Gen.string
       (Gen.opt Gen.small_string))
  |> make

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
  ]

let encrypt_test_suite =
  "encrypt test suite" >::: QCheck_runner.to_ounit2_test_list randomized_tests
